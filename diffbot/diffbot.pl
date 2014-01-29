%%  
%
%    Diffbot library
%
    
:- module( diffbot, [ 
			diffbot_defaults / 1, %% Get/Set default settings
			diffbot / 4,          %% Get Diffbot json using the default settings. 
			diffbot / 5,          %% Similar to diffbot / 4. Data is sent using HTTP POST
			                      %%
			                      %% Predicates ending in "_ex" do not use the default settings when connecting to endpoint. 
			diffbot_ex / 4,       %% Get Diffbot json
			diffbot_ex / 5,       %% Similar to diffbot_ex / 4.Data is sent using HTTP POST
			                      %%
			                      %% Utility predicates for handlind of urls and results 
			diffbot_url / 3,      %% Make Diffbot url with the default settings. 
			diffbot_url_ex / 3,   %% Make Diffbot url
			diffbot_error/3,      %% Succedes when json IS an error.
			show_json/1,          %% writes json to a html file in temp folder and opens it in browser. It is a quick way to inspect the JSON object
			open_url/1,           %% open url in browser. 
			to_atomic_url/2,      %% convert broken down format into a atomic format
			get_json/2,
			job_status/2,
			download_file/2
			]).

:- use_module( library( uri)).
:- use_module( library( http / http_open)).
:- use_module( library( http / json)).
:- use_module( library( http / http_header)). 
:- use_module( library( http / http_client)).
:- use_module( library(http/http_mime_plugin)).
:- use_module( library( www_browser)).

%global settings are stored using predicate diffs__ 
%:- dynamic( diffs__ / 1).
:- dynamic( diffs__ / 1).

:- ((diffs__(_),!); % do not empty arguments when reloading
     asserta( diffs__( []))).

/**
*  @form api_endpoint( +ApiName, -Url, -Arguments, -ExclusiveArguments)
*  @descr Read Api details. Succedes if ApiName exists.
*  ApiName can be one of article, frontpage, product, analyze, crawl, bulk, batch
*         a custom api is of form 'apiname'/custom
*  Url - endpoint url
*  Arguments accepted arguments
*  ExclusiveArguments when one of these arguments is present all other arguments except token and name are ignored 
*  @see broken down format used by http_open / 3
*  @see diffbot Api Arguments
*  @local
*/
api_endpoint( article, 
	[ protocol( http), host( 'api.diffbot.com'), path( '/v2/article')], 
	[ token / 1, url / 1, fields / 1, timeout / 1, callback / 1],[]).
api_endpoint( frontpage, 
    [ protocol( http), host( 'api.diffbot.com'), path( '/v2/frontpage')],
    [token / 1, url / 1, fields / 1, all / 0],[]).
api_endpoint( product, 
    [ protocol( http), host( 'api.diffbot.com'), path( '/v2/product')], 
    [token / 1, url / 1, fields / 1, timeout / 1, callback / 1],[]).
api_endpoint( image, 
    [ protocol( http), host( 'api.diffbot.com'), path( '/v2/image')],
    [token / 1, url / 1, fields / 1, timeout / 1, callback / 1],[]).
api_endpoint( analyze, 
    [ protocol( http), host( 'api.diffbot.com'), path( '/v2/analyze')], 
    [token / 1, url / 1, mode / 1, fields / 1, stats / 0],[]).
api_endpoint( crawl, 
    [ protocol( http), host( 'api.diffbot.com'), path( '/v2/crawl')], 
    [token / 1, name / 1, seeds / 1, apiUrl / 1, urlCrawlPattern / 1, urlCrawlRegEx / 1, 
    urlProcessPattern / 1, urlProcessRegEx / 1,
    pageProcessPattern / 1, maxToCrawl / 1, maxToProcess / 1, 
    restrictDomain / 1, notifyEmail / 1, notifyWebHook / 1, 
    crawlDelay / 1, repeat	 / 1, onlyProcessIfNew / 1, 
    maxRounds / 1, delete / 1, restart / 1, 
    pause / 1],[delete, pause, restart]).
api_endpoint( bulk, 
    [ protocol( http), host( 'api.diffbot.com'), path( '/v2/bulk')], 
    [token / 1, name / 1, urls / 1, apiUrl / 1, notifyEmail / 1, notifyWebHook / 1, 
    repeat / 1, maxRounds / 1,
    pageProcessPattern / 1, delete / 1, pause / 1],[delete, pause]).
api_endpoint( batch, 
    [ protocol( http), host( 'api.diffbot.com'), path( '/v2/batch')], 
    [token / 1, batch / 1, timeout / 1, callback / 1],[]).
api_endpoint( Custom/custom, 
    [ protocol( http), host( 'www.diffbot.com'), path( Path)], 
    [token / 1, timeout / 1],[]):- 
    uri_encoded(path,Custom,EncPath),
    atomic_concat('/api/',EncPath,Path).


/**
*  @form diffbot_defaults( ?Settings)
*  @descr get or set service access token
*  Ex: 
*    diffbot_defaults([token='c2e72x0c13708c75gbd8dua5b6ef8', timeout=3000]) - set the default token and timeout
*    diffbot_defaults(L) - get the list of global settings
*  @public
*/
diffbot_defaults( S) :- 
    is_list( S),
    ( retractall( diffs__( _));true), !,
    asserta( diffs__( S)).    
diffbot_defaults( R) :- var( R), diffs__( R).


/**
*  @form diffbot( +Url, +Arguments, -JSONObject,v2) is det.
*  @descr Get diffbot json. The Arguments are merged with the global settings.
*  v2 - constant atom for documenting in code which diffbot library was used
*  @public
*/
diffbot( Url, Arguments, JSONObject, v2) :- 
		endpoint_from_arguments(Url, Arguments, NewUrl),
        get_json(NewUrl, JSONObject).
        
        
/**
*  @form diffbot_ex( +Url, +Arguments, -JSONObject,v2) is det.
*  @descr Get diffbot json. Global settings are ignored
*  v2 - constant atom for documenting in code which diffbot library was used
*  @public
*/
diffbot_ex( Url, Arguments, JSONObject, v2) :- 
		endpoint_from_arguments_ex(Url, Arguments, NewUrl),
        get_json(NewUrl, JSONObject).    
 
/**
*  @form diffbot( +Url, +Data, +Arguments, -JSONObject,v2) is det.
*  @descr Similar to diffbot. Uses POST 
*  To post contents of a file 
*  the Data argument has to be file(your_file)
*  for other options @see http_post_data/4 in module(library(http/http_header))
*  @see http_post_data/4
*  @public
*/
diffbot( Url, Data, Arguments, JSONObject, v2):-
		endpoint_from_arguments(Url, Arguments, NewUrl),
        get_json_using_POST(NewUrl, Data, JSONObject).    

/**
*  @form diffbot_ex( +Url, +Data, +Arguments, -JSONObject,v2) is det.
*  @descr Similar to diffbot_ex. Uses POST
*  To post contents of a file 
*  the Data argument has to be file(your_file)
*  for other options @see http_post_data/4 in module(library(http/http_header))
*  @see http_post_data/4
*  @public
*/
diffbot_ex( Url, Data, Arguments, JSONObject, v2):-
		endpoint_from_arguments_ex(Url, Arguments, NewUrl),
        get_json_using_POST(NewUrl, Data, JSONObject).
    
/**
*  @form diffbot_error(+JSONObject, -ErrMsg, -ErrCode)
*  @descr Succedes when json IS an error. See example usage.
*  @public
*/
diffbot_error(JSONObject, ErrMsg, ErrCode):-
        JSONObject = json(L),
        is_list(L),!,
        ((member( error = ErrMsg,L) , member( errorCode = ErrCode , L));
         (member( error = ErrMsg,L) ; 
          member( errorCode = ErrCode , L))),!.
          
diffbot_error(_,'Invalid json', _).

/**
*  @form get_json( +Url, -J)
*  @private
*/
get_json(Url,J):-
        http_open( Url, In, []), 
        json_read( In, J),
        close( In),!. 
/**
*  @form get_json_using_POST( +Url, +Data, -J)
*  @private
*/
get_json_using_POST( Url, Data, J):-
        http_post( Url, Data, In,[]),
        (atom_json_term(In, J, []);J=In),!.        
        
/**
*  @form diffbot_url(+Url, +Arguments, -NewUrl)
%  @public
*/
diffbot_url(Url, Arguments, NewUrl):-
		endpoint_from_arguments(Url,Arguments,NewUrl),!.
        
/**
*  @form diffbot_url_ex(+Url, +Argument, -NewUrl)
*  @public
*/
diffbot_url_ex(Url, Arguments, NewUrl):-
		endpoint_from_arguments_ex(Url,Arguments,NewUrl),!.
		
        
/**
*  @form endpoint_from_arguments(+Url, +Argument, -NewUrl)
*  @desc 
*  @private
*/
endpoint_from_arguments(Url, Arguments,  EndUrl):-		
        diffs__( GArguments), 
        arguments_join(Arguments, GArguments, UArguments),
        endpoint_from_arguments_ex(Url, UArguments, EndUrl),!.
        
/**
*  @form endpoint_from_arguments_ex(+Url, +Argument, -NewUrl)
*  @desc Inner most predicate for combining arguments and url
*  @private
*/
endpoint_from_arguments_ex(Url, Arguments, [ search(  QsFiltered ) | EndUrl]):-
		var( Url),  !,
        member( api = Api, Arguments),       % get api
        api_endpoint( Api, EndUrl, Avail,ExclusiveArguments), % api url and arguments
        arguments_filter( Arguments, Avail, Qs),
        apply_exclusive_arguments(Qs,ExclusiveArguments,QsFiltered),!.
endpoint_from_arguments_ex(Url, Arguments, [ search(  [url = TargetUrl | QsFiltered] ) | EndUrl]):-
		to_atomic_url( Url, TargetUrl),!  ,
        member( api = Api, Arguments),       % get api
        api_endpoint( Api, EndUrl, Avail,ExclusiveArguments), % api url and arguments
        arguments_filter( Arguments, Avail, Qs),
        apply_exclusive_arguments(Qs,ExclusiveArguments,QsFiltered),!.
   
/**
*  @form arguments_join(+Arguments,+GArguments,-Result)
*
*  @descr Make a union betewen Arguments and GArguments
*  Values from Arguments have priority over values from GArguments
*
*  Ex:
*  arguments_join( [ n1=val1, n2=val2, n3], [ n2=foo, n4=val4], R).
*     R = [n1=val1, n2=val2, n3, n4=val4]
*  @private
*/
arguments_join( [], A, A).  % The union of the Empty Set and A is just set A
arguments_join( A, [], A).
arguments_join( [ N = V | T], G, [ N = V | T2]):- 
        delete( G, N = _, G2),
        delete( T, N = _, T1),!,
        arguments_join(T1,G2,T2).
arguments_join( [ Name | T], G, [ Name | T2]):- 
        delete( G, Name, G2),
        delete( T, Name, T1),!,
        arguments_join(T1,G2,T2).


/**
*  @form arguments_filter(+Arguments,+ToMatch,-Result)
*  @descr Intersects Arguments with ToMatch arguments
*  Ex:
*  arguments_filter( [ n1=val1, n2=val2, n3], [ n2/1, n4/0], R).
*     R = [n2=val2]
*  @private
*/
arguments_filter( _, [], []). % The intersection of A with the empty set is the empty set
arguments_filter( [], _, []).
arguments_filter( [(Name = Value)|T], Avail, [(Name = Value) |TQ]) :- 
		member(Name/1,Avail),!,
		delete(T,Name,T1),
		delete(T1,Name=_,T2),
        arguments_filter( T2, Avail, TQ).
arguments_filter( [Name|T], Avail, [(Name = '') |TQ]) :- 
		member(Name/0,Avail),!,
		delete(T,Name,T1),
		delete(T1,Name=_,T2),
        arguments_filter( T2, Avail, TQ).
arguments_filter( [_|T], Avail, Q) :-
        arguments_filter( T, Avail, Q).

/**
*  @form apply_exclusive_arguments(Qs,ExclusiveArguments,QsFiltered)
*  @desc the arguments in Qs that are closer to 
*     the head are more important because they are the last ones that 
*     the user has set
*     For some API when some arguments like start, delete are present
*     other arguments should not be present 
*     It works by examining the head until an argument different from 
*     name or token is found and it makes a decision what to keep or remove
*  @private
*/
apply_exclusive_arguments( L, [ ], L).% nothing to exclude
apply_exclusive_arguments( [ name=X | Qs], ExclusiveArguments, [ name=X | QsFiltered]):-
          apply_exclusive_arguments( Qs, ExclusiveArguments, QsFiltered).
apply_exclusive_arguments( [ token=X | Qs], ExclusiveArguments, [ token=X | QsFiltered]):-
          apply_exclusive_arguments( Qs, ExclusiveArguments, QsFiltered).
apply_exclusive_arguments( [ Name=X | Qs], ExclusiveArguments, [ Name=X | QsFiltered]):-
          member( Name, ExclusiveArguments), !, 
          keep_required( Qs, QsFiltered).
apply_exclusive_arguments( [ Name | Qs], ExclusiveArguments, [ Name | QsFiltered]):-
          remove_exclusive( Qs, ExclusiveArguments, QsFiltered).
             

/**
*  @form keep_required( +Arguments, -FilteredArguments).  
*  @desc remove from the list of arguments all arguments except name and token
*/
keep_required( [], []).
keep_required( [ name=X | T ], [ name=X | T2]):-!,keep_required(T,T2).
keep_required( [ token=X | T ], [ token=X | T2]):-!,keep_required(T,T2).
keep_required( [ _| T ],  T2):-keep_required(T,T2).

/**
*  @form remove_exclusive( +Arguments, +ExclusiveArguments, -FilteredArguments).  
*  @desc remove from the list of arguments all arguments except name and token
*/
remove_exclusive( [], _, []).
remove_exclusive( [ Name=_ | T ],ExclusiveArguments, FilteredArguments):-
    member( Name, ExclusiveArguments),!,
    remove_exclusive( T, ExclusiveArguments, FilteredArguments).
remove_exclusive( [ H | T ], ExclusiveArguments, [ H | T2]):-
    remove_exclusive(T, ExclusiveArguments,T2).

/**
*  @form show_json(+J)
*  @descr writes json to a html file in temp folder and opens it in browser.
*  @public
*/
show_json( J ):-tmp_file_stream( text,  File,  Stream ), 
        close( Stream ), 
        atomic_concat( File, '.html', File2 ), 
        rename_file( File, File2 ), 
        open( File2, write, S ), 
		write( S, '<!DOCTYPE html>' ), nl( S ), 
		write( S, '<html lang="en">' ), nl( S ), 
		write( S, '<head>' ), nl( S ), 
		write( S, '<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">' ), nl( S ), 
		write( S, '    <meta charset="utf-8">' ), nl( S ), 
		write( S, '  <link href="chrome://lightswitch_css/content/loff.css" rel="stylesheet" type="text/css">' ), nl( S ), 
		write( S, '<link href="chrome://lightswitch_css/content/loff.css" rel="stylesheet" type="text/css">' ), nl( S ), 
		write( S, '<style>' ), nl( S ), 
		write( S, 'code,  pre { padding: 3px 3px 2px 3px; margin :15px 75px 15px 75px;font-family: Menlo, Monaco, Consolas, "Courier New", monospace; font-size: 13px; color: rgb( 51,  51,  51 ); border-radius: 3px; }' ), nl( S ), 
		write( S, '</style>' ), nl( S ), 
		write( S, '</head>' ), nl( S ), 
		write( S, '<body>' ), nl( S ), 
		write( S, '  <pre class="code"> ' ), nl( S ), 
		json_write( S, J ), nl( S ), 
		write( S, '  </pre>' ), nl( S ), 
		write( S, '</body>' ), nl( S ), 
		write( S, '</html>' ), nl( S ), 
		close( S ), 
		open_url( File2 ).
		
/**
*  @form open_url( +Url )
*  @descr open url in browser. It has alternatives when built in www_open_url/1 fails.
*  @public
*/		
open_url( Url ):-www_open_url( Url ), !.
open_url( Url ):-atomic_concat( 'firefox ', Url, Cmd ), shell( Cmd, 0 ), !.
open_url( Url ):-atomic_concat( 'google-chrome ', Url, Cmd ), shell( Cmd, 0 ).

/**
*  @form to_atomic_url(+Url, R)
*  @descr make atom from url
*
*  Ex:
*  to_atomic_url( 'http://www.google.com', R)
*    R= http://google.com
*  to_atomic_url( [host(http), host('www.diffbot.com'),path('/products/automatic/classifier/')], R)
*    R= 'http://www.diffbot.com/products/automatic/classifier/'
*  @see broken-down format defined in library(http\http_open)
*  @private
*/
to_atomic_url( R, R) :- atomic( R), !.
to_atomic_url( X, R) :- 
       ( member( protocol( P), X);P=http), 
       ( ( member( port( PNum), X), atomic_concat( ':', PNum, Port));Port=''), 
       ((member( host( H), X), atomic_list_concat( [P, '://', H, Port],Root));Root=''),
       ( member( path( URI), X); URI=''),
       ( ( member( search( Q), X), uri_query_components( Qenc, Q), atomic_concat( '?', Qenc, Qs)); Qs=''),
       atomic_list_concat( [Root, URI, Qs], R).
       

job_status(json(L),S):-%write(L),show_json(json(L)),
          member(jobs=L1,L),
          member(json(L2),L1),
          is_list(L2),   
          member(jobStatus=json(Stat),JList),
          member(status=S,Stat).

download_file(Url_src,Dest_name_path):-
          open(Dest_name_path,write,StreamOut),
         http_open(Url_src,StreamIn,[]),
          copy_stream_data(StreamIn,StreamOut),
        close( StreamIn),
          close( StreamOut).
