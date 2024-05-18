-module(accounts_manager).
-export([accounts_management/1]).

accounts_management(Accounts) ->
    receive
        {create_account, From, User, Pwd} ->
            NewAccounts = create_account(Accounts, From, User, Pwd),
            accounts_management(NewAccounts);
        {delete_account, From, User, Pwd} ->
            %NewAccounts = delete_account(Accounts, From, User, Pwd),
            %accounts_management(NewAccounts);
        {login, From, User, Pwd} ->
            %NewAccounts = login(Accounts, From, User, Pwd),
            %accounts_management(NewAccounts);
        {logout, From, User, _} ->
            %NewAccounts = logout(Accounts, From, User),
            %accounts_management(NewAccounts)
    end.


create_account(Accounts, From, User, Pwd) ->
    case maps:find(User, Accounts)  of
        {ok,_} ->
            From ! {user_exists, accounts_manager}, % pass user_exists to the requester
            NewAccounts = Accounts;                 % newAccounts stay the same
        _ ->
            NewAccounts = maps:put(User, Pwd, Accounts), % add the new user to the list
            From ! {success, accounts_manager}           % pass success to the requester
    end,
    NewAccounts. % return the new list of accounts
    



