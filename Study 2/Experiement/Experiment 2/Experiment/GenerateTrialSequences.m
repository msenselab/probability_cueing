for n=1:24
   
    exp = CExp(1,[8, 2, 2, 60],'blockRepetition',1);
    N = length(exp.seq);
    
    high_prob_region=mod(n,2); % Set high prob region to 1=top or 0=bottom depending on odd or even participant number
    
    for i=1:exp.maxTrls
        if exp.seq(i,4) <= 20 % Distractor absent: 20=1/3*60
            exp.seq(i,4)=0; 
        elseif exp.seq(i,4) <= 56 %% High probability region 56 = 20 + 0.9*40
            exp.seq(i,4)=ceil((exp.seq(i,4)-20)/9)+high_prob_region*4; % 1-4 or 5-8 depndending on odd or even participant number
        else % Low probability region
            exp.seq(i,4) = exp.seq(i,4)-56+(1-high_prob_region)*4; % 5-8 or 1-4 depndending on odd or even participant number
        end    
    end
        
    seq=exp.seq;    
    
    for i=1:length(seq) % Check for cases where target and distractor location are the same
        if seq(i,1)==seq(i,4) % Target and distractor location are the same
            if seq(i,1)>4
                pos=5:8;
                pos(seq(i,4)-4)=[];
            else
                pos=1:4;
                pos(seq(i,4))=[];
            end
            seq(i,1)=pos(ceil(rand*3)); % Randomly choose a different position for the target within the same (high/low) region
        end
    end
    
    save(['sequences/seq_' num2str(n) '.mat'], 'seq')
    
end
