% Diffbot API article example.
%
%  USAGE
%  =====
%  Assign a valid values for token, crawl job and csv file name . 
%  type  consult('/media/9CC4E676C4E651D6/Documents/workspace/prolog/crawl_job.pl'). in the prolog console
%  type download_crawl_db('/tmp/mycrawl.csv'). to chech job status and download to '/tmp/mycrawl.csv' when complete
%  download the results in csv format

:-use_module(library(diffbot)).
:-use_module(library('http/json')).

download_crawl_db(CsvFile):-Token='c2e72be8c7c0c1370535b6ef8bbd8daa',
       Cname = mycrawl,
          diffbot(_,
          [api=crawl,name=Cname,token=Token],J,v2),!,
          job_status(J,_),
          atomic_list_concat(['http://api.diffbot.com/v2/crawl/download/',Token,'-',Cname,'_urls.csv'],Url),
          download_file(Url,CsvFile).

