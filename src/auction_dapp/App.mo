import Array "mo:base/Array";
import Hash "mo:base/Hash";
import HashMap "mo:base/HashMap";
import Heap "mo:base/Heap";
import Iter "mo:base/Iter";
import Nat "mo:base/Nat";
import Option "mo:base/Option";
import Order "mo:base/Order";
import Principal "mo:base/Principal";
import Prelude "mo:base/Prelude";
import Time "mo:base/Time";
import Text "mo:base/Text";


import Balances "./Balances";
import Types "./Types";

actor class App(balancesAddr: Principal) = App {

    type Auction = Types.Auction;
    type AuctionId = Types.AuctionId;
    // Distributed System module4
    type Bid = Types.Bid;
    type BidProof = Types.BidProof;
    type HashedBid = Hash.Hash;

    type Item = Types.Item;
    type Result = Types.Result;
    type UserId = Types.UserId;
    type UserState = Types.UserState;

    let balances = actor (Principal.toText(balancesAddr)) : Balances.Balances;

    let auctions = HashMap.HashMap<AuctionId, Auction>(1, Nat.equal, Hash.hash);

    // Distributed Systems module3
    let userStates = HashMap.HashMap<UserId, UserState>(1, Principal.equal, Principal.hash);

    // Distributed Systems module4
    let hashedBids = HashMap.HashMap<AuctionId, [HashedBid]>(1, Nat.equal, Hash.hash);

    // Used to create unique auctionIds in startAuction()
    var auctionCounter = 0;

    /// Query functions:

    public query func getAuctions() : async ([(AuctionId, Auction)]) {
        let entries = auctions.entries();
        Iter.toArray<(AuctionId, Auction)>(entries)
    };

    /// Update functions:

    /// Creates a new item and corresponding auction.
    /// Args:
    ///     |owner|     The UserId of the auction owner.
    ///     |name|      The item's name.
    ///     |description|   The item's description.
    ///     |url|       The url the auction can be access at.
    ///     |startingBid|  The starting price of the item
    public func startAuction(
        owner: UserId,
        name: Text,
        description: Text,
        url: Text,
        startingBid: Nat
    ) {
        let item = makeItem(name, description, url);
        let auction = makeAuction(owner, item, startingBid);
        auctions.put(auctionCounter, auction);
        auctionCounter += 1;
    };


    /// Records a new user bid for an auction.
    /// Args:
    ///   |bidder|     The UserId of the bidder.
    ///   |auctionId|  The id of the auction.
    ///   |amount|     The user's bit amount.
    /// Returns:
    ///   A Result indicating if the bid was successfully processed (see "Error" in Types.mo for possible errors).
    public func makeBid(bidder: UserId, auctionId: AuctionId, amount: Nat) : async (Result) {
        let balance = await balances.getBalance(bidder);
        if (amount > balance) return #err(#insufficientBalance);

        let auctionCheck = auctions.get(auctionId);
        switch (auctionCheck) {
            case (null) {
                #err(#auctionNotFound)
            };
            case (?auction) {
                // Distributed System module2
                if (Time.now() > auction.ttl) { return #err(#auctionExpired) };

                switch (acquireLock(bidder, auctionId, auction)) {
                    case (#err(e)) #err(e);
                    case (#ok) {
                        switch (auction.highestBidder) {
                            case (null) {
                                auctions.put(auctionId, setNewBidderAndGet(auction, amount, bidder));
                                #ok()
                            };
                            case (?prviousHighestBidder) {
                                if (amount > auction.highestBid) {
                                    let myPrincipal = Principal.fromActor(App);
                                    ignore balances.transfer(bidder, myPrincipal, amount);
                                    ignore balances.transfer(myPrincipal, prviousHighestBidder, auction.highestBid);
                                    auctions.put(auctionId, setNewBidderAndGet(auction, amount, bidder));
                                    #ok()
                                } else {
                                    #err(#belowMinimumBid) /// FIXME
                                }
                            };
                        }
                    };
                }
                
            };
        }
    };

    /// Helper method used to create a new item (used in startAuction).
    /// Args:
    ///   |name|         The item's name.
    ///   |description|  The item's description.
    ///   |url|          The URL the auction can be accesses at.
    /// Returns:
    ///   The newly created Item (see Item in Types.mo)
    func makeItem(name: Text, description: Text, url: Text) : (Item) {
        {
            name = name;
            description = description;
            url = url;
        }
    };

    /// Helper method used to create a new item (used in auctionItem).
    /// Args:
    ///   |owner|         The auction's owner.
    ///   |item|          The item object.
    ///   |startingBid|   The starting bid of the auction.
    /// Returns:
    ///   The newly created Auction (see Auction in Types.mo)
    func makeAuction(
        owner: UserId,
        item: Item,
        startingBid: Nat
    ) : (Auction) {
        {
            owner = owner;
            item = item;
            highestBid = startingBid;
            highestBidder = null;
            ttl = Time.now() + (3600 * 1000_1000_1000);
            lock = owner;
            lock_ttl = 0;
        }
    };

    /// Helper method used to set a new highest bidder in an auction and return new Action instance(used in makeBid).
    /// Args:
    ///   |auction|  The auction id.
    ///   |bidder|   The highest bidder's Principal id.
    ///   |bid|      The highest bid of the auction.
    /// Returns:
    ///   The new updated Auction (see Auction in Types.mo)
    func setNewBidderAndGet(auction: Auction, bid: Nat, bidder: Principal) : (Auction) {
        {
            owner = auction.owner;
            item = auction.item;
            highestBid = bid;
            highestBidder = ?bidder;
            ttl = auction.ttl;
            lock = auction.lock;
            lock_ttl = auction.lock_ttl;
        }
    };

    /// setNewLock() Helper method used to set a new highest bidder in an auction (used in acquireLock())
    /// Args:
    ///     |auction|   The Auction being updated
    ///     |lockAcquirer|  The id of the user acquiring the lock
    /// Returns:
    ///     The updated Auction
    func setNewLock(auction: Auction, lockAcquirer: UserId) : (Auction) {
        {
            owner = auction.owner;
            item = auction.item;
            highestBid = auction.highestBid;
            highestBidder = auction.highestBidder;
            ttl = auction.ttl;
            lock = lockAcquirer;
            lock_ttl = Time.now() + (3600 * 1000_1000);
        }
    };

    /// acquireLock() Helper a "lock" in a user's name for a particular Auction, preventing other users from
    /// bidding on the auction for a short time (used in makeBid())
    /// Args:
    ///     |id|    The UserId of the user acquiring the lock
    ///     |auctionId| The id of the auction
    ///     |auction|   The auction itself
    /// Returns:
    ///     A Result indicating if the lock was successfully acquired 
    func acquireLock(
        id: UserId,
        auctionId: AuctionId,
        auction: Auction
    ) : (Result) {
        if (id == Option.unwrap(auction.highestBidder)) {
            #err(#highestBidderNotPermitted)
        } else if (Time.now() > auction.lock_ttl) {
            auctions.put(auctionId, setNewLock(auction, id));
            #ok()
        } else {
            #err(#lockNotAcquired)
        }
    };

    // Distributed Systems module3
    /// makeNewUserState() Helper method used to initialize a new UserState.
    /// Returns:
    ///     A UserState with a starting |seq| of 0 and empty |bids| heap.
    func makeNewUserState() : (UserState) {
        {
            var seq = 0;
            bids = Heap.Heap<Bid>(bidOrd);
        }
    };

    /// bidOrd(x, y) Helper method used to order the bids in the UserState heap (used in makeNewUserState())
    /// Args:
    ///     |x| the first Bid
    ///     |y| the second Bid
    /// Returns:
    ///     A Motoko Order variant type: either #less or #greater.
    func bidOrd(x: Bid, y: Bid) : (Order.Order) {
        if (x.seq < y.seq) #less else #greater
    };

    /// getSeq(userId) Helper method used to retrieve the current |seq| of a user
    /// used in both User.mo's makeQueuedBid() and App.mo's makeQueuedBid().
    /// Args:
    ///     |userId|    The UserId of the specified user.
    /// Returns:
    ///     A UserState.seq in the form of the Nat
    public func getSeq(userId: UserId) : async (Nat) {
        switch (userStates.get(userId)) {
            case (null) {
                userStates.put(userId, makeNewUserState());
                0
            };
            case (?userState) {
                userState.seq
            };
        }
    };

    /// putBid(userId, bid) Helper method used to place a bid in a user's userState.
    /// Args:
    ///     |userId|    The UserId of the specified User
    ///     |bid|       The specified Bid
    func putBid(userId: UserId, bid: Bid) : () {
        switch (userStates.get(userId)) {
            case (null) Prelude.unreachable();
            case (?userState) {
                userState.bids.put(bid);
                userState.seq := bid.seq;
            };
        }
    };

    /// makeQueuedBid(bid) Called by User to queue a |bid|
    /// Args:
    ///     |bid|   The Bid to be queued
    /// Returns:
    ///     A Result indicating if the the bid was successfully queued
    public shared(msg) func makeQueuedBid(bid: Bid) : async (Result) {
        let seq = await getSeq(msg.caller);
        if (bid.seq > seq) {
            putBid(msg.caller, bid);
            #ok()
        } else {
            #err(#seqOutOfOrder)
        }
    };

    /// processBids() Called by Users to process all the current bids stored in their UserState.
    /// Returns:
    ///     A Result indicating if the bids were successfully processed
    public shared(msg) func processBids() : async (Result) {
        switch (userStates.get(msg.caller)) {
            case (null) return #err(#userNotFound);
            case (?userState) {
                loop {
                    switch (userState.bids.peekMin()) {
                        case (null) { return #ok() };
                        case (?bid) {
                            ignore await makeBid(msg.caller, bid.auctionId, bid.amount)
                        };
                    };

                    userState.bids.deleteMin();
                };
            };
        }
    };

    /// Distributed Systems module4
    /// makeHashedBid(auctionId, hashedBid) Add the |hashedBid| to an auctions's array of bids in HashedBids
    /// Args:
    ///     |auctionId| The id of the auction.
    ///     |hashedBid| The hashed result of the bid to be submitted.
    /// Returns:
    ///     A Result indicating if the lock was successfully acquired
    public shared(msg) func makeHashedBid(
        auctionId: AuctionId,
        hashedBid: HashedBid
    ) : async (Result) {
        switch (auctions.get(auctionId)) {
            case (null) #err(#auctionNotFound);
            case (?auction) {
                if (Time.now() > auction.ttl) { return #err(#auctionExpired) };

                hashedBids.put(
                    auctionId, 
                    Array.append<HashedBid>(
                        [hashedBid],
                        switch (hashedBids.get(auctionId)) {
                            case (null) [];
                            case (?hashedBidsArr) hashedBidsArr;
                        }
                    )
                );

                #ok() 
            };
        }
    };


    /// proofHash(bidProof)  Helper method used to create the hash of the BidProof
    /// Args:
    ///     |bidProof|  The BidProof to be hashed 
    /// Returns:
    ///     A Hash of the |salt| append to the |amount|
    func proofHash(bidProof: BidProof) : Hash.Hash {
        Text.hash(Nat.toText(bidProof.amount) # bidProof.salt)
    };

    /// processHashedBids(auctionId, auction, bidder, amount)
    /// Helper method used in publishBidProof() to process bids once the bidder has chosen to publish their bid proof.
    /// Args:
    ///     |auctionId| The AuctionId of the auction for which the bids belong to.
    ///     |auction|   The Auction itself.
    ///     |bidder|    The UserId of the bidder
    ///     |amount|    The bid amount
    /// Returns:
    ///     A Result indicating if the bids were successfully processed
    func processHashedBids(
        auctionId: AuctionId,
        auction: Auction,
        bidder: UserId,
        amount: Nat
    ) : async (Result) {
        switch (auction.highestBidder) {
            case (null) {
                auctions.put(auctionId, setNewBidderAndGet(auction, amount, bidder));
                #ok()
            };
            case (?previousHighestBidder) {
                if (amount > auction.highestBid) {
                    let myPrincipal = Principal.fromActor(App);
                    ignore balances.transfer(bidder, myPrincipal, amount);
                    ignore balances.transfer(
                        myPrincipal, 
                        previousHighestBidder,
                        auction.highestBid
                    );

                    auctions.put(auctionId, setNewBidderAndGet(auction, amount, bidder));
                    #ok()
                } else {
                    #err(#belowMinimumBid)
                }
            };
        }
    };

    /// publishBidProof(auctionId, AuctionId, bidProof) Call by a user once an auction is over to "reveal" their bids
    /// Args:
    ///     |auctionId| The id of the auction.
    ///     |bidProof|  The BidProof to be published.
    /// Returns:
    ///     A Result indicating if the lock was successfully acquired
    public shared(msg) func publishBidProof(
        auctionId: AuctionId,
        bidProof: BidProof
    ) : async (Result) {
        switch (auctions.get(auctionId)) {
            case (null) #err(#auctionNotFound);
            case (?auction) {
                if (Time.now() < auction.ttl) { return #err(#auctionStillActive) };

                let proof = proofHash(bidProof);
                switch (Array.find<HashedBid>(
                    switch (hashedBids.get(auctionId)) {
                        case (null) [];
                        case (?hashedBidArr) hashedBidArr;
                    },
                    func (elem: HashedBid) : Bool { Hash.equal(elem, proof) }
                )) {
                    case (null) #err(#bidHashNotSubmitted);
                    case (_) {
                        await processHashedBids(auctionId, auction, msg.caller, bidProof.amount)
                    };
                }
            };
        }
    };
};