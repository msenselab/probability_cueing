for n=1:24
   
    exp = CExp(1,[8, 6, 1, 60, 1],'blockRepetition',1);
    N = length(exp.seq);
    
    high_prob_region=mod(n,2); % Set high prob region to 1=top or 0=bottom depending on odd or even participant number

    tar_shape = repmat(1:2, 1, N/2);
    tar_shape = tar_shape(randperm(length(tar_shape)));
    tar_ori = repmat(1:2, 1, N/2);
    tar_ori = tar_ori(randperm(length(tar_ori)));
    color = repmat(0:1, 1, N/2);
    color = color(randperm(length(color)));
    
    for i=1:exp.maxTrls
        exp.seq(i,2) = tar_shape(i);
        exp.seq(i,3) = tar_ori(i);
        if exp.seq(i,4) <= 20 % Distractor absent: 20=1/3*60
            exp.seq(i,4)=0; 
        elseif exp.seq(i,4) <= 56 %% High probability region 56 = 20 + 0.9*40
            exp.seq(i,4)=ceil((exp.seq(i,4)-20)/9)+high_prob_region*4; % 1-4 or 5-8 depndending on odd or even participant number
        else % Low probability region
            exp.seq(i,4) = exp.seq(i,4)-56+(1-high_prob_region)*4; % 5-8 or 1-4 depndending on odd or even participant number
        end
        exp.seq(i,5) = color(i);       
    end
        
    seq=exp.seq;    
        
    for i=1:length(seq) % Check for cases where target and distractor location are the same
        if seq(i,1)==seq(i,4) % Target and distractor location are the same
            if seq(i,4)>4               
                pos=5:8;
                pos(seq(i,4)-4)=[];
            else
                pos=1:4;
                pos(seq(i,4))=[];
            end
            seq(i,1)=pos(ceil(rand*3));            
        end
    end
    
    save(['sequences/seq_' num2str(n) '.mat'], 'seq')
    
end
