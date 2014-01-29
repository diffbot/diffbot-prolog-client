% Diffbot API frontpage example.
%
%  USAGE
%  =====
%  Assign a valid value for token 
%  type  consult('/media/9CC4E676C4E651D6/Documents/workspace/prolog/example_frontpage.pl'). in the prolog console

:-use_module(library(diffbot)).
:-diffbot_defaults([token='c2e72be8cx0c1370535b6ef8bbd8daa']).
:-diffbot_defaults([token='c2e72be8c7c0c1370535b6ef8bbd8daa']).
:-diffbot('http://www.xconomy.com/san-francisco/2012/07/25/diffbot-is-using-computer-vision-to-reinvent-the-semantic-web/',
          [api=frontpage],J,v2),show_json(J).
