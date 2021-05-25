import Array "mo:base/Array";
import HashMap "mo:base/HashMap";
import Principal "mo:base/Principal";
import Option "mo:base/Option";

import Types "./Types";

shared(msg) actor class Balances() {
    
    type UserId = Principal;
    type Result = Types.Result;

    // Store the Balances canister creator.
    let owner = msg.caller;

    // let userIdWithBalances = HashMap.HashMap<UserId, Nat>(1, Principal.equal, Principal.hash);
    /// HashMap can't be stable var
    stable var userIdWithBalances: [(UserId, Nat)] = [];

    /// Query functions

    /// Retrieves the balance of the give |user|. Accessible to external canisters.
    /// Args:
    ///     |user|  The UserId of the user whose balance we want to access.
    /// Returns:
    ///     A Nat representing the user balance
    public query func getBalance(user: UserId): async (Nat) {
        _getBalanceElseZero(user)
    };

    /// Update functions

    /// Transfers money between users (used for payment after transactions).
    /// Args:
    ///     |from|  The UserId of the user whose balance we transfer from.
    ///     |to|    The UserId of the user whose balance we transfer to.
    ///     |amount|    The amount of money to be transfered.
    /// Returns:
    ///     return #err(#insufficientBalance if the balance of from is less than amount, or #err(#userNotFound) if the from is not exists
    ///     return #ok() if the transfer is success.
    public shared(msg) func transfer(
        from: UserId,
        to: UserId,
        amount: Nat
    ) : async (Result) {
        assert( from != to );
        switch (_getBalance(from)) {
            case (null) {
                #err(#userNotFound)
            };
            case (?balance) {
                if (balance < amount) {
                    #err(#insufficientBalance)
                } else {
                    userIdWithBalances := Array.map<(UserId, Nat), (UserId, Nat)>(userIdWithBalances, func ((userId, balance)) { 
                        if (userId == from) { 
                            (userId, balance - amount) 
                        } else if (userId == to) {
                            (userId, balance + amount)
                        } else {
                            (userId, balance)
                        }
                    });

                    #ok()
                };
              
            };
        }
        
    };

    /// Deposit money into a user account. Used to initialize user balances upon start of the application.
    /// Args:
    ///     |user|  The UserId of the user whose balance we deposit to
    ///     |amount|    The amountof money to be deposited.
    public shared(msg) func deposit(user: UserId, amount: Nat) : async () {
        assert (owner == msg.caller);
        
        switch (_getBalance(user)) {
            case (null) {
                userIdWithBalances := Array.append<(UserId, Nat)>(userIdWithBalances, Array.make<(UserId, Nat)>((user, amount)));
            };
            case (?balance) {
                userIdWithBalances := Array.map<(UserId, Nat), (UserId, Nat)>(userIdWithBalances, func ((userId, balance)) { 
                    if (user == userId) { (userId, balance + amount) }
                    else { (userId, balance) };
                });
            };
        };
        
    };

    /// Retrieves the balance of the given |user|. Used as a helper function for other methods in this actor.
    /// This is the internal-facing version. See getBalance for the version accessible to external canisters
    /// Args:
    ///     |user|  The UserId of the user whose balance we want to access.
    /// Returns:
    ///     return an Option, if the user is exists, return ?balance, else return ull
    func _getBalance(user: UserId): (?Nat) {
        let userWithBalance: ?(UserId, Nat) = Array.find<(UserId, Nat)>(userIdWithBalances, func (userWith) { userWith.1 == user });
        Option.map<(UserId, Nat), Nat>(userWithBalance, func ((userId, balance)) { balance })
    };

    /// Retrieves the balance of the given |user|. Used as a helper function for other methods in this actor
    /// This is the internal-facing version. See getBalance for the version accessible to external canisters 
    /// Args:
    ///     |user|  The UserId of the user whose balance we want to access.
    /// Returns:
    ///     A Nat representing the user balance, if the user is not exists, return 0
    func _getBalanceElseZero(userId: UserId): (Nat) {
        switch (_getBalance(userId)) {
            case (null) 0;
            case (?balance) balance;
        }
    };
};