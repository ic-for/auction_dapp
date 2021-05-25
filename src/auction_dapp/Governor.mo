import Array "mo:base/Array";
import Float "mo:base/Float";
import Principal "mo:base/Principal";
import Result "mo:base/Result";
import Time "mo:base/Time";

import Types "./Types";

shared(msg) actor class Governor(starterApp: Principal, voteThresold: Float) {
    type UserId = Types.UserId;
    type Proposal = Types.Proposal;
    type ProposalStatus = Types.ProposalStatus;
    type Vote = Types.Vote;

    type Result = Result.Result<(), Types.GovError>;
    type PropResult = Result.Result<ProposalStatus, Types.GovError>;

    let owner = msg.caller;

    var currentApp = starterApp;

    /// This should use BigMap  #FIXME
    var proposals: [var Proposal] = [var];

    /// Query functions:

    public query func getCurrentApp() : async (UserId) {
        currentApp
    };

    /// Update functions:

    /// Swaps in the new App canister to replace the previous version. This is the result of 
    ///     a migration proposal succeeding.
    /// Args:
    ///     |propNum|   The number representing the passed proposal.
    /// Returns:
    ///     A Result indcicating if the migration was successfully executed (see "GovError" in Types.mo).
    public shared(msg) func migrate(propNum: Nat) : async (Result) {
        switch (_checkProposal(propNum)) {
            case (#ok(status)) {
                switch (status) {
                    case (#succeeded) {
                        currentApp := proposals[propNum].newApp;
                        #ok()
                    };
                    case (_) #err(#proposalNotActive);
                }
            };
            case (#err(e)) #err(e);
        }
    };


    /// Creates a new proposal for App migration.
    /// Args:
    ///     |newApp| The Principal id of the new proposed App.
    /// Returns:
    ///     The id associated with this proposal.
    public shared(msg) func propose(newApp: UserId) : async (Nat) {
        proposals := Array.thaw<Proposal>(
            Array.append<Proposal>(Array.freeze<Proposal>(proposals), [makeProposal(newApp, msg.caller)])
        );
        proposals.size();
    };

    /// Cancels a proposal (can only be called by the owner of the proposal or owner of the Governor).
    /// Args:
    ///     |propNum| The id of the proposal.
    /// Returns:
    ///     A Result indicating if the cancellation was successfully executed (see "GovError" in Types.mo).
    public shared(msg) func cancelProposal(propNum: Nat) : async (Result) {
        if (proposals.size() < propNum) return #err(#proposalNotFound);

        let prop = proposals[propNum];
        if (msg.caller != owner and msg.caller != prop.proposer) return #err(#incorrectPermissions);

        switch (prop.status) {
            case (#active) {
                prop.status := #canceled;
                proposals[propNum] := prop;
                #ok()
            };
            case (_) #err(#proposalNotActive);
        }
    };


    /// External-facing method for inter-canister calls
    public func checkProposal(propNum: Nat) : async (PropResult) {
        _checkProposal(propNum)
    };

    /// Records a new proposal vote.
    /// Args:
    ///     |propNum|   The id of the proposal.
    ///     |vote|  The vote being case (variant type Vote - see Types.mo)
    /// Returns:
    ///     A Result indicating if the vote was successfully recorded (see "GovError in Types.mo")
    public func voteOnProposal(propNum: Nat, vote: Vote) : async (Result) {
        switch (_checkProposal(propNum)) {
            case (#ok(status)) {
                switch (status) {
                    case (#active) {
                        let prop = proposals[propNum];
                        switch (vote) {
                            case (#inFavor) prop.votesFor += 1;
                            case (#against) prop.votesAgainst += 1;
                        };
                        proposals[propNum] := prop;
                        #ok()
                    };
                    case (_) #err(#proposalNotActive);
                    
                }
            };
            case (#err(e)) #err(e);
        }
    };

    /// Checks if the given proposal has exceeded its time to live.
    /// If so, checks if the vote has passed the give threshold.
    /// Args:
    ///     |propNum|   The id of the proposal.
    /// Returns:
    ///     The new status of this proposal (in the form of the PropResult variant - see Types.mo)
    func _checkProposal(propNum: Nat) : (PropResult) {
        if (proposals.size() < propNum) return #err(#proposalNotFound);

        let prop = proposals[propNum];
        let status = switch (prop.status) {
            case (#active) {
                if (Time.now() > prop.ttl) {
                    let outcome = Float.div(
                        Float.fromInt(prop.votesFor),
                        (Float.fromInt(prop.votesFor) + Float.fromInt(prop.votesAgainst))
                    );

                    if (outcome > voteThresold) {
                        prop.status := #succeeded;
                        proposals[propNum] := prop;
                        #succeeded
                    } else {
                        prop.status := #defeated;
                        proposals[propNum] := prop;
                        #defeated
                    }
                } else {
                    #active
                }
            };
            case (anythingElse) anythingElse;
        };

        #ok(status)
    };

    /// Helper called in Propose - used to create a new proposal
    /// Args:
    ///     |newApp|    The Principal of the newly proposed App.
    ///     |proposer|  The canister making the proposal.
    /// Returns:
    ///     The new Proposal object (see Types.mo)
    func makeProposal(newApp: UserId, proposer: UserId) : (Proposal) {
        {
            newApp = newApp;
            proposer = proposer;
            var votesFor = 0;
            var votesAgainst = 0;
            var status: ProposalStatus = #active;
            ttl = Time.now() + (3600 * 1000_1000_1000);
        }
    };
};