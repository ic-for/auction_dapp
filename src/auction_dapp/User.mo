import Principal "mo:base/Principal";

import App "./App";
import Balances "./Balances";
import Governor "./Governor";
import Types "./Types";

actor class User(govAddr: Principal, balancesAddr: Principal) = User {
    type AuctionId = Types.AuctionId;

    let balances = actor (Principal.toText(balancesAddr)) : Balances.Balances;
    let governor = actor (Principal.toText(govAddr)) : Governor.Governor;

    func me() : Principal {
        return Principal.fromActor(User);
    };

    func getCurrentApp(): async (App.App) {
        let currentAppAddr = await governor.getCurrentApp();
        let currentApp = actor (Principal.toText(currentAppAddr)) : App.App;
    };

    public func makeQueuedBid(auctionId: AuctionId) : async () {
        
        let currentApp = await getCurrentApp();

        let seqNum = (await currentApp.getSeq(me())) + 1;
        ignore await currentApp.makeQueuedBid({
            seq = seqNum;
            amount = await balances.getBalance(me());
            auctionId = auctionId;
        });

        ignore await currentApp.processBids();
    };

    public func startAuction(
        name: Text,
        description: Text,
        url: Text,
        startingBid: Nat
    ) : async () {
        
        let currentApp = await getCurrentApp();

        currentApp.startAuction(me(), name, description, url, startingBid);
    };
}