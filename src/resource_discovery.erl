-module(resource_discovery).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0,add_target_resource_type/1,add_local_resource/2,fetch_resources/1,trade_resources/0]).
-record(state,{target_resource_types,local_resource_tuples,found_resource_tuples}).
-define(SERVER,?MODULE).

start_link() ->
	gen_server:start_link({local,?SERVER}, ?MODULE, [], []).

add_target_resource_type(Type) ->
	gen_server:cast(?SERVER	, {add_target_resource_type,Type}).

add_local_resource(Type,Instance) ->
	gen_server:cast(?SERVER, {add_local_resource,{Type, Instance}}).

fetch_resources(Type) ->
	gen_server:call(?SERVER,{fetch_resources,Type}).

trade_resources() ->
	gen_server:cast(?SERVER, trade_resources).
%% ====================================================================
%% Behavioural functions 
%% ====================================================================

%% init/1
init([]) ->
    {ok, #state{target_resource_types=[],
				local_resource_tuples=dict:new(),
				found_resource_tuples=dict:new()}}.



%% handle_call/3
handle_call({fetch_resources,Type},_From,State) ->
	{reply,dict:find(Type, State#state.found_resource_tuples),State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
handle_cast({add_target_resource_type,Type},State) ->
	TargetTypes = State#state.target_resource_types,
	NewTargetTypes = [Type|lists:delete(Type, TargetTypes)],
	{noreply,State#state{target_resource_types=NewTargetTypes}};
handle_cast({add_local_resource,{Type, Instance}},State) ->
	ResourceTuples = State#state.local_resource_tuples,
	NewResourcetuples = add_resource(Type,Instance,ResourceTuples),
	{noreply,State#state{local_resource_tuples=NewResourcetuples}};
handle_cast(trade_resources,State) ->
	ResourceTuples = State#state.local_resource_tuples,
	AllNodes = [node()|nodes()],
	lists:foreach(
		fun(Node) ->
			gen_server:cast({?SERVER,Node},{trade_resources,{node(),ResourceTuples}})
		end,
		AllNodes),
	{noreply,State};
handle_cast({trade_resources,{ReplyTo,Remotes}},
			 #state{local_resource_tuples = Locals,
				 	target_resource_types = TargetTypes,
				 	found_resource_tuples = OldFound}=State) ->
	FilteredRemotes = resource_for_types(TargetTypes,Remotes),
	NewFound = add_resource(FilteredRemotes,OldFound),
    case ReplyTo of
        noreply ->
            ok;
        _ ->
            gen_server:cast({?SERVER,ReplyTo},{trade_resources,{noreply,Locals}})
    end,
    {noreply,State#state{found_resource_tuples=NewFound}};
handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
handle_info(_Info, State) ->
    {noreply, State}.


%% terminate/2
terminate(Reason, State) ->
    ok.


%% code_change/3
code_change(OldVsn, State, Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================
add_resource([{Type,Resource}|T],ResourceTuples) ->
	add_resource(T,add_resource(Type,Resource,ResourceTuples));
add_resource([],ResourceTuples) ->
	ResourceTuples.
add_resource(Type,Resource,ResourceTuples) ->
	case dict:find(Type, ResourceTuples) of
		{ok,ResourceList} ->
			NewList = [Resource|lists:delete(Resource, ResourceList)],
			dict:store(Type, NewList, ResourceTuples);
		error ->
			dict:store(Type, [Resource], ResourceTuples)
	end.

resource_for_types(Types,ResourceTuples) ->
	Fun =
		fun(Type,Acc) ->
			case dict:find(Type,ResourceTuples) of
				{ok,List} ->
					[{Type,Instance}||Instance <- List] ++ Acc;
				error ->
					Acc
			end
		end,
	lists:foldl(Fun,[],ResourceTuples).