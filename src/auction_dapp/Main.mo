import Option "mo:base/Option";
import Debug "mo:base/Debug";
import Prim "mo:prim";
import Principal "mo:base/Principal";
import Result "mo:base/Result";

import App "./App";
import Balances "./Balances";
import Governor "./Governor";
import Types "./Types";

actor {
    type Result<S, T> = Result.Result<S, T>;

    type App = App.App;
    type Balances = Balances.Balances;
    type Governor = Governor.Governor;
    type GovError = Types.GovError;
    type AuctionId = Types.AuctionId;
    type Auction = Types.Auction;

    var app: ?App = null;
    var balances: ?Balances = null;
    var governor: ?Governor = null;

    // deployBalances() Performs inital setup operations by instantiating the Balances, App, and Governor canisters
    public shared(msg) func deployBalances() : async () {
        switch (balances) {
            case (?bal) Debug.print("Balances Already deployed");
            case _ {
                let tempBalances = await Balances.Balances();
                await tempBalances.deposit(msg.caller, 100);
                balances := ?tempBalances;
            };
        }
    };

    // deployApp()
    public func deployApp() : async () {
        switch (app, balances) {
            case (?a, _) Debug.print("App Already deployed");
            case (_, null) Debug.print("Should call deployBalances() first");
            case (_, ?bal) {
                let tempApp = await App.App(Principal.fromActor(bal));
                tempApp.startAuction(
                    Principal.fromActor(tempApp),
                    "auction dapp",
                    "auction description .......",
                    "",
                    100
                );

                app := ?tempApp;
            };
        }
    };

    // deployGovernor()
    public func deployGovernor() : async () {
        switch (governor, balances) {
            case (?gov, _) Debug.print("Governor Already deployed");
            case (_, null) Debug.print("Should call deployBalances() first");
            case (_, ?bal) {
                governor := ?(await Governor.Governor(Principal.fromActor(bal), 0.5));
            };
        }
    };

    // deployAll() replies immediately after initiating but not awaiting the asynchronous deployments
    public func deployAll() : async () {
        ignore async {
            await deployBalances();
            ignore deployApp();
            ignore deployGovernor();
        };
    };

    // isReady() replies promptly (and is a cheap query)
    public query func isReady() : async Bool {
        switch (balances, app, governor) {
            case (? _, ? _, ? _) true;
            case _ false;
        }
    };

    // getActions() Get all auctions from app canister
    public func getAuctions() : async ([(AuctionId, Auction)]) {
        switch (app) {
            case (null) throw Prim.error("Should call deployApp() first");
            case (?a) { await a.getAuctions() };
        }
    };

    // migrate(propNum) migrate governor delegate to governoer canister
    public shared(msg) func migrate(propNum: Nat) : async (Result<(), GovError>) {
        /// FIXME Option map function is too cumbersome, else use map function is good.
        switch (governor) {
            case (null) #err(#noGovernor);
            case (?gov) (await gov.migrate(propNum));
        };
    };

    // propose(newApp) migrate governor delegate to governoer canister
    public shared(msg) func propose(newApp: Principal) : async (Result<Nat, GovError>) {
        switch (governor) {
            case (null) #err(#noGovernor);
            case (?gov) #ok(await gov.propose(newApp));
        };
    };

    // voteForProp(propNum) migrate governor delegate to governoer canister
    public shared(msg) func voteForProp(propNum: Nat) : async (Result<(), GovError>) {
        switch (governor) {
            case (null) #err(#noGovernor);
            case (?gov) (await gov.voteOnProposal(propNum, #inFavor));
        };
    };

    // voteAgainstProp(propNum) migrate governor delegate to governoer canister
    public shared(msg) func voteAgainstProp(propNum: Nat) : async (Result<(), GovError>) {
        switch (governor) {
            case (null) #err(#noGovernor);
            case (?gov) (await gov.voteOnProposal(propNum, #against));
        };
    };
};