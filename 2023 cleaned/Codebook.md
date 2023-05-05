# Codebook of Variables

Here is a list of variables that can be found in the dataset:

1. **order_of_trial**: Indicates the order of the trial, which can be either 1 (without any training), 2 (during training), or 3 (for participants who have passed the training).

2. **coalesced_gamble**: Contains the first gamble presented to the participant in coalesced form.

3. **coalesced_gamble1**: Contains the second gamble presented to the participant in coalesced form.

4. **response**: Indicates the participant's selection of either gamble 0 (coalesced_gamble) or gamble 1 (coalesced_gamble1).

5. **RT**: Participant's reaction time.

6. **sub_no**: Participant ID.

7. **condition**: This variable indicates the experimental condition, which can be one of three types:
    
    a. (A) Coalesced - Identical: 1 trial G- vs G+ → Instruction G- vs G+ → 1 trial G- vs G+
    
    b. (B) Coalesced - Different: 1 trial F- vs F+ → Instruction F- vs F+ → 1 trial G- vs G+
    
    c. (C) Transparent: 1 trial GS- vs GS+

8. **ip**: Participant's IP address.

9. **verification**: Participant's verification code.

10. **experimentDuration**: Duration of the experiment.

11. **training_received**: Details the training received by participants in Conditions A or B, distinguishing the position of the gamble: F_minus vs F_plus, F_plus vs F_minus, G_minus vs G_plus, or G_plus vs G_minus.

12. **training_version**: Based on training_received, takes the value "F" (for F_minus vs F_plus or F_plus vs F_minus) or "G" (for G_minus vs G_plus or G_plus vs G_minus).

13. **training_F**: Takes the value 1 if the training is "F".

14. **training_G**: Takes the value 1 if the training is "G".

15. **any_training**: Takes the value 1 if any training was received before the gamble.

16. **toggle_count**: Indicates the number of clicks on the toggle button.

17. **gamble_played_condition**: Indicates the condition under which the gamble was played. It can be one of the following:

    a. (A) G+ vs G-
    
    b. (B) F+ vs F-
    
    c. (B) G+ vs G-
    
    d. (C) GS+ vs GS-

18. **gamble_played**: Indicates the type of gamble played, which can be either "G", "F", or "GS".

19. **gamble_played_d**: Contains the longer name of the gamble played. It can be "G+ vs G-", "F+ vs F-", or "GS+ vs GS-".

20. **gamble_played_1**: Takes the value 1 if the gamble played was "F".

21. **gamble_played_2**: Takes the value 1 if the gamble played was "G".

22. **gamble_played_3**: Takes the value 1 if the gamble played was "GS"

23. **G_and_training**:  Gamble G in any condition after training.

24. **G_and_no_training**: Gamble G in any condition before training.

25. **odd**: Equal to 1 when there is an odd number of clicks on the toggle button.

26. **toggle_count2**: Number of clicks squared.

27. **toggle_count_upto10**: Number of clicks capped to 10 clicks.

28. **training_FG**: Distinguishes the gamble played during training, F or G.

39. **duplicate_id**: Equal to 1 when the participant had a duplicated IP.

30. **omit_ip**: Equal to 1 when the participant had a duplicated IP.

31. **gamble_cond_n**: Equal to gamble_played_condition (but in Stata is a numerical variable).

32. **logRT**: Log of RT.


