# Diffbot API SWI-Prolog client

## Installation

1. Choose a folder for the diffbot.pl module. 

2. Open the personal prolog initialization file.

3. To install diffbot as library append the code bellow to the prolog initialization file.

```
:- multifile(library_directory/1).
library_directory('diffbot dir').
```
Don't forget to replace 'diffbot dir' with the correct folder :).

On Linux look for ~/.plrc. If it does not exist you can create it.

On Windows to edit the personal initialization file select Settings\User init file from console menu bar. You may need to use two backslashes to specify a path on windows. Close, restart prolog and choose save file when prompted.

That's it, Diffbot is now installed as a module library.

## Configuration

Add Diffbot to you program using:
```
:-use_module(library(difbot)).
```	
It is recommended to set a global value for the access token:
```
:-diffbot_defaults([token='yourtokenhere']).
```

You can add in the list of global values any other argument except for the url:
```
:-diffbot_defaults([token='yourtokenhere', timeout=6000]).
```

Global values will cumulate to the API call arguments.

## Middleware

No third party library is required

## Usage

The base form of an API call is
```
diffbot(+Url,+ArgList,-JSONObject,+v2).
```
`ArgList` is in the same format as the global values list.


API uses [library(http/http_open)](http://www.swi-prolog.org/pldoc/man?section=httpopen).
The format for url and POST data parameters is the same as for [http_post/4](http://www.swi-prolog.org/pldoc/doc_for?object=http_post/4).
You can also see these parameters in the examples.

### Article API

Using the article API
 
```

:-use_module(library(diffbot)).
:-diffbot_defaults([token=..]).

diffbot('http://www.xconomy.com/san-francisco/2012/07/25/diffbot-is-using-computer-vision-to-reinvent-the-semantic-web/',[api=article, fields='icon,url'],J,v2),show_json(J).
```
Calling 'show_json/1' will show the json in a webbrowser and it is optional.

### Frontpage API

Using the frontpage API 

```ruby
:-use_module(library(diffbot)).
:-diffbot_defaults([token=..]).
diffbot('http://www.xconomy.com/san-francisco/2012/07/25/diffbot-is-using-computer-vision-to-reinvent-the-semantic-web/',[api=frontpage],J,v2)
```

### POSTing an article 

```
:-use_module(library(diffbot)).
:-diffbot_defaults([token=..]).

diffbot('http://www.diffbot.com',file('/home/..path_atom.txt'),[api=article],J,v2),show_json(J).
```
You can replace `file('/home/..path_atom')` with any data supported by [http_post/4](http://www.swi-prolog.org/pldoc/doc_for?object=http_post/4).


### Custom API

All diffbot calls must contain an api argument which is the api name in atom format.
In case of the custom api argument is of the form `custom_api_name/custom`:
```
diffbot(SomeUrl,[api=custom_api_name/custom],J,v2)
```
So 
```
diffbot(SomeUrl,[api=article],J,v2)
```
is an article API call and 
```
diffbot(SomeUrl,[api=article/custom],J,v2)
```
is a custom API call

### Crawlbot API

Most of the Crawllbot API calls are similar to Article api calls.
The difference is that a Crawllbot has a status and a database of results associated.
To code bellow can be use to obtain the Crawl job status .
```
diffbot(_,[api=crawl,name=mycrawl],L,V2),json_write(current_output,L).
```
Note that the URL isnot used for viewing status.

A Crawl job also takes pause/restart/delete commands.

This code 
```
diffbot(_,
          [api=crawl,pause=1,name=mycrawl, fields='icon,url'],J),to_atomic_url(J,L).
```
will pause the crawl job

Using the utility predicates you can also preview the URL generated for Crawl boot
```
diffbot_url(_,
          [api=crawl,pause=1,name=mycrawl, fields='icon,url'],J),to_atomic_url(J,L).
```

-Initial Commit by Cristian Teglas-
