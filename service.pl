:- module(server, [server/1]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module('rules.pl').
:- use_module('configuration.pl').
:- http_handler(root(start), initial, []).
:- http_handler(root(cfg),handle_cfg,[prefix]).

server(Port) :-
        http_server(http_dispatch, [port(Port)]).

say_hi(_Request) :-
        format('Content-type: text/plain~n~n'),
        format('Hello World!~n').

initial(_Request):-
  start(Cfg),
  json_conf(Json,Cfg),
  reply_json(Json).

request_cfg(Request,Cfg,Atom):-
	member(path_info(P),Request),
	atom_concat('/',Atom,P),
	term_to_atom(Code,Atom),
	conf_code(Cfg,Code).

handle_cfg(Request):-
	(   member(method(post),Request)
	->  handle_cfg_post(Request)
	;   handle_cfg_get(Request)
	).

handle_cfg_get(Request):-
	request_cfg(Request,Cfg,Atom),
	json_conf(Json,Cfg),
	reply_json(json([code=Atom,configuration=Json])).

handle_cfg_post(Request):-
	http_read_data(Request,Atom,[to(atom)]),
	request_cfg(Request,Cfg,_),
	handle_command(Cmd,Cfg)



