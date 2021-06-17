import Hash "mo:base/Hash";
import Heap "mo:base/Heap";
import Nat "mo:base/Nat";
import Result "mo:base/Result";

module {
    public type AuctionId = Nat;
    public type UserId = Principal;
    public type Result = Result.Result<(), Error>;

    // Distributed Systems module3
    public type Bid = {
        seq: Nat;
        amount: Nat;
        auctionId: AuctionId;
    };

    public type UserState = {
        var seq: Nat;
        bids: Heap.Heap<Bid>;
    };

    // Distributed Systems module4
    public type BidProof = {
        amount: Nat;
        salt: Text;
    };

    public type Auction = {
        owner: UserId;
        item: Item;
        highestBid: Nat;
        highestBidder: ?UserId;
        ttl: Int;
        // Distributed System module2
        lock: UserId;
        lock_ttl: Int;
    };

    public type Item = {
        name: Text;
        description: Text;
        url: Text;
    };

    public type ProposalStatus = {
        #active;
        #canceled;
        #defeated;
        #succeeded;
    };

    public type Proposal = {
        newApp: Principal;
        proposer: Principal;
        var votesFor: Nat;
        var votesAgainst: Nat;
        var status: ProposalStatus;
        ttl: Int;
    };

    public type Error = {
        #belowMinimumBid;
        #insufficientBalance;
        #auctionNotFound;
        #userNotFound;
        // Distributed Systems module2
        #highestBidderNotPermitted;
        #lockNotAcquired;
        #auctionExpired;
        
        // Distributed Systems module3
        #seqOutOfOrder;

        // Distributed System module4
        #auctionStillActive;
        #bidHashNotSubmitted;
    };

    public type Vote = {
        #inFavor;
        #against;
    };

    public type GovError = {
        #noGovernor;
        #incorrectPermissions;
        #proposalNotFound;
        #proposalNotActive;
    };
};
