for n=1:24
   
    exp = CExp(1,[8, 2, 2, 90],'blockRepetition',1);
    
    freq_dist_pos = ceil(n/3); % frequent distractor position (1-8)
    
    for i=1:exp.maxTrls
        if exp.seq(i,4) <= 30
            exp.seq(i,4)=0; % Distractor absent
        elseif exp.seq(i,4) <= 69 %% 69 = 30 + 0.65*60
            exp.seq(i,4)=freq_dist_pos; % Distractor in frequent position
        else
            pos = 1:8;
            pos(freq_dist_pos) = []; % Remove frequent position
            exp.seq(i,4) = pos(ceil((exp.seq(i,4)-69)/3)); % select one as distractor location
        end
    end
    
    seq=exp.seq;    
    
    p1_dist=randperm(60);
    p2_dist=randperm(60);
    practice_dist_abs_tarpos=repmat(1:8,1,5);
    practice_dist_abs_tarpos=practice_dist_abs_tarpos(randperm(length(practice_dist_abs_tarpos)));
    practice_dist_pres_tarpos=repmat(1:8,1,10);    
    practice_dist_pres_tarpos=practice_dist_pres_tarpos(randperm(length(practice_dist_pres_tarpos)));    
    for i=1:60
        practice1(i,2)=ceil(rand*2); % Target shape in first practice block
        practice2(i,2)=ceil(rand*2); % Target shape in second practice block
        practice1(i,3)=ceil(rand*2); % Line orientation in first practice block
        practice2(i,3)=ceil(rand*2); % Line orientation in second practice block
        
        if p1_dist(i) <= 20 
            practice1(i,4)=0; % Distractor absent
        elseif p1_dist(i) <= 46 % 46 = 20 + 0.65*40
            practice1(i,4)=freq_dist_pos; % Distractor in frequent position
        else
            pos = 1:8;
            pos(freq_dist_pos) = []; % Remove frequent position
            practice1(i,4) = pos(ceil((p1_dist(i)-46)/2)); % select one as distractor location
        end
        
        if p2_dist(i) <= 20
            practice2(i,4)=0; % Distractor absent
        elseif p2_dist(i) <= 46 % 46 = 20 + 0.65*40
            practice2(i,4)=freq_dist_pos; % Distractor in frequent position
        else
            pos = 1:8;
            pos(freq_dist_pos) = []; % Remove frequent position
            practice2(i,4) = pos(ceil((p2_dist(i)-46)/2)); % select one as distractor location
        end
        
    end
    practice1(practice1(:,4)==0,1)=practice_dist_abs_tarpos(1:20);
    practice2(practice2(:,4)==0,1)=practice_dist_abs_tarpos(21:40);
    practice1(practice1(:,4)>0,1)=practice_dist_pres_tarpos(1:40);
    practice2(practice2(:,4)>0,1)=practice_dist_pres_tarpos(41:80);
    
        
    seq=[practice1; seq(1:1440,:); practice2; seq(1441:end,:)]; % Add pratice blocks at start of each session

    color=[zeros(1500,1); ones(1500,1)];
    color=color(randperm(length(color)));    
    seq=[seq, color]; % Add random distractor color
    
    pos=1:8;
    pos(freq_dist_pos)=[];
    targetpos=[pos(1)*ones(250,1); pos(2)*ones(250,1); pos(3)*ones(250,1); pos(4)*ones(250,1); 
        pos(5)*ones(250,1); pos(6)*ones(250,1); pos(7)*ones(250,1)]; 
    targetpos=targetpos(randperm(length(targetpos)));
    targetpos=[freq_dist_pos*ones(250,1); targetpos];
    ind = find(seq(:,4)>0 & seq(:,4)~=freq_dist_pos);
    targetpos_infreq=targetpos(1:700);
    targetpos_freq=targetpos(701:end);
    targetpos_infreq=targetpos_infreq(randperm(length(targetpos_infreq)));
    seq(ind,1)=targetpos_infreq;
    seq(seq(:,4)==freq_dist_pos,1)=targetpos_freq;
    temp_seq=seq(ind,:);
    for i=1:700 % Check the relevant sequence for cases where target and distractor location are the same
        if(temp_seq(i,1)==temp_seq(i,4)) % Target and distractor location are the same   
            j=1;
            while(temp_seq(i,4)==temp_seq(j,1) || temp_seq(j,4) == temp_seq(i,1)) % Find a trial with a different target location
                j=j+1;
            end
            temp=temp_seq(i,1); % swap the two target locations
            temp_seq(i,1)=temp_seq(j,1);
            temp_seq(j,1)=temp;
        end
    end
    seq(ind,:)=temp_seq;
    
    save(['sequences/seq_' num2str(n) '.mat'], 'seq')
    
end
