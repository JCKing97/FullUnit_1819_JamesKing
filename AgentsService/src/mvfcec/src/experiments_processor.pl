/*
	Author: Kostas Stathis
	Date: 10-Dec-17
	Description: Processor of experiments.

        Given a directory of domains containing experimental data in sub-directories

        domains
		domain1
			exp1
				datafile1 <-- data containing events and times they were observed.
				datafile2 <-- "        "        "     "    "    "    "     "
				...
				datafileK <-- "        "        "     "    "    "    "     "
			exp2
			..
			expL
		domain2
			exp1
			exp2
			...
			expM
		...
		domainN 
		        ...
	
	this files defines a processor that takes the files of an experiment within a domain and
        compiles it mvis that can be processed by mvfCEC.
*/

% run_experiment(+Domain, +Experiment, +KB).
% To run an +Experiment within a +Domain using a domain specific +KB.
% The experiment is run by compiling all the observations recorded in
% the experimental data to construct the maximal validity intervals.
% The domain specific +KB is a list of files containing the knowledge
% of a +Domain shared to perform the different experiments.

run_experiment(Domain, Experiment, KB):-
	get_domain_experiment_dirs(Domain, Experiment, DomainDir, ExpDir),
	consult_domain_kb(KB, DomainDir),
	experimental_data(Domain, Experiment, DataFilePrefix, FromFileIndex, ToFileIndex),
	compile_experimental_data(ExpDir, DataFilePrefix, FromFileIndex, ToFileIndex).




% get_domain_experiment_dirs(+Domain, +Experiment, -DomainDir, -ExpDir).
% Construct paths for the directories of the domain and the experiment
% from the names of the domain and the experiment.

get_domain_experiment_dirs(Domain, Experiment, DomainDir, ExpDir):-
	domains_dir(TopDomainDir),
	l_list_atomconcat([TopDomainDir, Domain, /], DomainDir),
	l_list_atomconcat([DomainDir, Experiment, /], ExpDir).




% consult_domain_kb(+KBFiles, +DomainDir).
% Consult the domain specific knowledge bases needed for the experiment.

consult_domain_kb([], _).
consult_domain_kb([KBFile1|KBFiles], DomainDir):-
	l_atomconcat(DomainDir, KBFile1, PathKBFile1),
	l_reconsult(PathKBFile1),!,
	consult_domain_kb(KBFiles, DomainDir).



% compile_experimental_data(+Dir, +Name, +From, +To).
% Compile all files in +Dir prefixed with +Name and ending 
% with a number. Start from number +From and end with number
% +To, incrementing by one for each compilation. If there
% is only one file to be compiled +From = +To.

compile_experimental_data(_, _, From, To):-
	From > To,!,
	(From = 0
	->
	l_write('compile_experimental_data/5: Variable "To" must be >= 0.\n')
	;
	l_writelist(['\n<END>--- Compilation ended: ', From, ' files compiled for this experiment.', '\n'])
	).
compile_experimental_data(Dir, Name, From, To):-
	next_datafile(Dir, Name, From, NextFileName),
	process_file_no(From, NextFileName),
	NewFrom is From + 1,!,
	compile_experimental_data(Dir, Name, NewFrom, To).



% next_datafile(Dir, Name, No, FileName).
% Construct the path for +FileName, by concatenating +Dir, with the +Name and the +No.

next_datafile(Dir, Name, No, FileName):-
	l_list_atomconcat([Dir, Name, No], FileName).



% process_file_no(+No, +DataFileNo).
% Process file number +No using its path +DataFileNo to
% compile all the observations it contains..

process_file_no(No, DataFileNo):-
	l_reconsult(DataFileNo),
	l_writelist(['\n', No,': compiling narrative file ',DataFileNo,'....\n\n']),
	process_updates(DataFileNo).
	



% process_updates(+DataFileNo).
% Update processing starts here using an interfaces to the mvfCEC.
% - DataFileNo: the file no of the experiment currently being processed.

process_updates(DataFileNo):-
        input_format(ObsFormat, E, T),
        l_retract(ObsFormat),                      %remove observation from memory
        write(E), write('@'), write(T), write(':'),
        time(update_at(E, T)),                     %perform update - event E is happening at T (and time it)
        !,
        process_updates(DataFileNo).
process_updates(DataFileNo):-
        write('\nNarrative in:'),
        write(DataFileNo),
        write(' has been processed.\n').
