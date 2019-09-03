function as_search_session2

    exp = CExp(1,[8, 2, 2, 90],'blockRepetition',1); 
       
    exp.subInfo('group');  % acquire subject information
    exp.sName = [exp.sName '_s2'];
    load(['sequences/seq_' num2str(exp.sPara) '.mat'])
    exp.seq=seq(961:1920,:);
    exp.maxTrls = size(exp.seq,1);
    
    as_search_main(exp);
    
end
   
