% complete training program for all programs

% trainingprogram(G,A,Lst)
% where G is input for goal (stength, size, or endurance)
% where A is input for experience in years (non-negative integers; 0,1,2,...)
% experience between 0 and 1 years inclusive redirected to beginner program
trainingprogram(G,A,Lst):-
	-1 < A,
	A < 2,
	fullbody(G,Lst).
	
% experience between 2 and 4 years inclusive redirected to intermediate program
trainingprogram(G,A,Lst):-
    1 < A,
    A < 5,
	intermediate(G,Lst).

% note: assuming someone has started working out at 18 years old, 
%       the program is suitable up to approximately 40 years old,
%       hence the upper bound for experience is 22 years
% experience between 5 and 22 years inclusive redirected to advanced program
trainingprogram(G,A,Lst):-
	4 < A,
	A < 23,
	ppl(G,Lst).

%bodyDay(G,A,Lst) is true if Lst contains all the exercises on day A of the fullbody program with goal G
bodyDay(G,A,Lst):- 
	compound(G,A,X),
	accessory(A,Y),
	append(X,Y,Lst).

%compound(G,A,Lst) is true if Lst contains the compound movements given the day A and goal G
compound(G,A,Lst):-findall([X,S],exercise(X,A,S,G),Lst).

%accessory(A,Lst) is true if Lst contains the accessory movements given the day A
accessory(A,Lst):-findall([X,S],exercise(X,A,S),Lst).

%fullbody(G,Y) is true if Y is the one week training program with performance goal G, and the REST DAY in Y is the day off of the gym
fullbody(G,Lst):-
	bodyDay(G,fba,A),
	bodyDay(G,fbb,B),
	bodyDay(G,fbc,C),
	append(A,['REST DAY'],X),
	append(B,['REST DAY'],Y),
	append(C,['REST DAY'],Z),
	append(X,Y,T),
	append(T,Z,V),
	append(V,['REST DAY'],Lst).

% compound exercise under the fullbody DAY A with strength goal
%fullbody(A,str)
exercise(squat,fba,'3x3_5',str).
exercise(inclineBenchPress,fba,'3x3_5',str).
exercise(pullup,fba,'4x8_12').
exercise(seatedLegCurl,fba,'4x8_12').
exercise(wideGripLatPullDown,fba,'4x8_12').
exercise(shoulderPress,fba,'4x8_12').
exercise(frontRaise,fba,'4x8_12').

% compound exercise under the fullbody DAY A with size goal
%fullbody(A,size)
exercise(squat,fba,'3x8_12',size).
exercise(inclineBenchPress,fba,'3x8_12',size).

% compound exercise under the fullbody DAY A with endurance goal
%fullbody(A,endurance)
exercise(squat,fba,'3x15_20',end).
exercise(inclineBenchPress,fba,'3x15_20',end).


% compound exercise under the fullbody DAY B with strength goal
%fullbody(B,str)
exercise(legPress,fbb,'4x8_12').
exercise(benchPress,fbb,'3x3_5',str).
exercise(barbellRow,fbb,'3x3_5',str).
exercise(seatedLegExtension,fbb,'4x8_12').
exercise(closeGripLatPullDown,fbb,'4x8_12').
exercise(lateralRaise,fbb,'4x8_12').
exercise(reverseFly,fbb,'4x8_12').

% compound exercise under the fullbody DAY B with size goal
%fullbody(B,size)
exercise(benchPress,fbb,'3x8_12',size).
exercise(barbellRow,fbb,'3x8_12',size).

% compound exercise under the fullbody DAY B with endurance goal
%fullbody(B,endurance)
exercise(benchPress,fbb,'3x15_20',end).
exercise(barbellRow,fbb,'3x15_20',end).


% compound exercise under the fullbody DAY C with strength goal
%fullbody(C,str)
exercise(lunges,fbc,'4x8_12').
exercise(dumbbellPress,fbc,'4x8_12').
exercise(deadLift,fbc,'3x3_5',str).
exercise(seatedCalfRaise,fbc,'4x8_12').
exercise(cableRow,fbc,'4x8_12').
exercise(overheadShoulderPress,fbc,'3x3_5',str).
exercise(dumbbellFlies,fbc,'4x8_12').

% compound exercise under the fullbody DAY C with size goal
%fullbody(C,size)
exercise(deadLift,fbc,'3x8_12',size).
exercise(overheadShoulderPress,fbc,'3x8_12',size).

% compound exercise under the fullbody DAY C with endurance goal
%fullbody(C,endurance)
exercise(deadLift,fbc,'3x15_20',end).
exercise(overheadShoulderPress,fbc,'3x15_20',end).

%intermediate(G,Y) is true if Y is a full week training split with intermediate experienve and perfomance goal G
intermediate(G,Lst):-
	bodyDay(G,ua,XA),
	bodyDay(G,ub,B),
	bodyDay(G,la,XC),
	bodyDay(G,lb,D),
	append(XA,['END OF WORKOUT'],A),
	append(A,B,X),
	append(XC,['END OF WORKOUT'],C),
	append(['REST DAY'],C,Y),
	append(X,Y,Z),
	append(D,['REST DAY'],J),
	append(Z,J,H),
	append(H,['REST DAY'],Lst).

%Upper(A,str)
exercise(benchPress,ua,'3x3_5',str).
exercise(inclineDumbbellPress,ua,'4x8_12').
exercise(barbellRow,ua,'3x3_5',str).
exercise(wideGripLatPullDown,ua,'4x8_12').
exercise(frontRaise,ua,'4x8_12').
exercise(tricepBarPushDown,ua,'4x8_12').
exercise(preacherCurl,ua,'4x8_12').
exercise(reverseFly,ua,'4x8_12').

exercise(benchPress,ua,'3x8_12',size).
exercise(barbellRow,ua,'3x8_12',size).

exercise(benchPress,ua,'3x15_20',end).
exercise(barbellRow,ua,'3x15_20',end).


%Lower(A,str)
exercise(squat,la,'3x3_5',str).
exercise(seatedLegExtension,la,'4x8_12').
exercise(seatedLegCurl,la,'4x8_12').
exercise(seatedCalfRaise,la,'4x8_12').
exercise(rackPull,la,'3x3_5',str).
exercise(backExtension,la,'4x8_12').
exercise(abCrunch,la,'4x8_12').
exercise(oblique,la,'4x8_12').

exercise(squat,la,'3x8_12',size).
exercise(rackPull,la,'3x8_12',size).

exercise(squat,la,'3x15_20',end).
exercise(rackPull,la,'3x15_20',end).


%Upper(B,str)
exercise(inclineBenchPress,ub,'3x3_5',str).
exercise(dumbbellPress,ub,'4x8_12').
exercise(dumbbellRow,ub,'4x8_12').
exercise(cableRow,ub,'4x8_12').
exercise(overheadShoulderPress,ub,'3x3_5',str).
exercise(lateralRaise,ub,'4x8_12').
exercise(hammerCurl,ub,'4x8_12').
exercise(tricepDumbbellExtension,ub,'4x8_12').

exercise(inclineBenchPress,ub,'3x8_12',size).
exercise(overheadShoulderPress,ub,'3x8_12',size).

exercise(inclineBenchPress,ub,'3x15_20',end).
exercise(overheadShoulderPress,ub,'3x15_20',end).


%Lower(B,str)
exercise(deadLift,lb,'3x3_5',str).
exercise(legPress,lb,'4x8_12').
exercise(seatedLegExtension,lb,'4x8_12').
exercise(seatedLegCurl,lb,'4x8_12').
exercise(standingCalfRaise,lb,'4x8_12').
exercise(romanianDeadLift,lb,'3x3_5',str).
exercise(hangingLegRaise,lb,'4x8_12').
exercise(declineSitup,lb,'4x8_12').

exercise(deadLift,lb,'3x8_12',size).
exercise(romanianDeadLift,lb,'3x8_12',size).

exercise(deadLift,lb,'3x15_20',end).
exercise(romanianDeadLift,lb,'3x15_20',end).

%ppl(G,Y) is true if Y is a full week training split for advanced lifters, with performance goal G
ppl(G,Lst):-
	bodyDay(G,pa,XA),
	bodyDay(G,pb,XB),
	bodyDay(G,pc,C),
	bodyDay(G,pd,XD),
	bodyDay(G,pe,E),
	append(XA,['END OF WORKOUT'],A),
	append(XB,['END OF WORKOUT'],B),
	append(A,B,X),
	append(C,['REST DAY'],Y),
	append(XD,['END OF WORKOUT'],D),
	append(D,E,Z),
	append(X,Y,H),
	append(H,Z,J),
	append(J,['REST DAY'],Lst).

%PPL(A,str)
exercise(benchPress,pa,'3x3_5',str).
exercise(inclineDumbbellPress,pa,'4x8_12').
exercise(cableFly,pa,'4x8_12').
exercise(shoulderPress,pa,'4x8_12').
exercise(tricepBarPushDown,pa,'4x8_12').
exercise(tricepRopePushDown,pa,'4x8_12').
exercise(tricepSkullcrusher,pa,'4x8_12').
exercise(lateralRaise,pa,'4x8_12').
exercise(frontRaise,pa,'4x8_12').

exercise(benchPress,pa,'3x8_12',size).
exercise(benchPress,pa,'3x8_12',end).


%PPL(B,str)
exercise(deadLift,pb,'3x3_5',str).
exercise(barbellRow,pb,'3x3_5',str).
exercise(wideGripLatPullDown,pb,'4x8_12').
exercise(cableRow,pb,'4x8_12').
exercise(dumbbellShrug,pb,'4x8_12').
exercise(ropeFacePull,pb,'4x8_12').
exercise(reverseFly,pb,'4x8_12').
exercise(preacherCurl,pb,'4x8_12').
exercise(hammerCurl,pb,'4x8_12').

exercise(deadLift,pb,'3x8_12',size).
exercise(barbellRow,pb,'3x8_12',size).

exercise(deadLift,pb,'3x15_20',end).
exercise(barbellRow,pb,'3x15_20',end).


%PPL(C,str)
exercise(squat,pc,'3x3_5',str).
exercise(seatedHamstringCurl,pc,'4x8_12').
exercise(seatedLegExtension,pc,'4x8_12').
exercise(seatedCalfRaise,pc,'4x8_12').
exercise(lunges,pc,'4x8_12').
exercise(romanianDeadLift,pc,'3x3_5',str).
exercise(declineSitup,pc,'4x8_12').
exercise(abCrunch,pc,'4x8_12').
exercise(oblique,pc,'4x8_12').

exercise(squat,pc,'3x8_12',size).
exercise(romanianDeadLift,pc,'3x8_12',size).

exercise(squat,pc,'3x15_20',end).
exercise(romanianDeadLift,pc,'3x15_20',end).


%PPL(D,str)
exercise(benchPress,pd,'3x3_5',str).
exercise(inclineDumbbellPress,pd,'4x8_12').
exercise(wideGripLatPullDown,pd,'4x8_12').
exercise(tBarRow,pd,'3x3_5',str).
exercise(overheadShoulderPress,pd,'3x3_5').
exercise(tricepRopePushDown,pd,'4x8_12').
exercise(tricepDumbbellExtension,pd,'4x8_12').
exercise(standingBarbellCurl,pd,'4x8_12').
exercise(lateralRaise,pd,'4x8_12').

exercise(benchPress,pd,'3x8_12',size).
exercise(tBarRow,pd,'3x8_12',size).

exercise(benchPress,pd,'3x15_20',end).
exercise(tBarRow,pd,'3x15_20',end).


%PPL(E,str)
exercise(legPress,pe,'4x8_12').
exercise(rackPull,pe,'3x3_5',str).
exercise(seatedLegExtension,pe,'4x8_12').
exercise(seatedHamstringCurl,pe,'4x8_12').
exercise(standingCalfRaise,pe,'4x8_12').
exercise(frontSquat,pe,'4x8_12').
exercise(backExtension,pe,'4x8_12').
exercise(abCrunch,pe,'4x8_12').
exercise(hangingLegRaise,pe,'4x8_12').

exercise(rackPull,pe,'3x8_12',size).
exercise(rackPull,pe,'3x15_20',end).

% append(A,B,C) is true if C contains the elements of A followed by the elements of B
append([],L,L).
append([H|T],L,[H|R]) :-
    append(T,L,R).

% calculator for bmr and daily calories

% https://www.calculator.net/bmr-calculator.html
% https://www.calculator.net/calorie-calculator.html

% Gender is male or female
% Weight in kilograms (kg)
% Height in centimetres (cm)
% Age in years 

% Using Mifflin St Jeor BMR estimation formula
%bmr(Gender, Weight, Height, Age, BMR) is true if BMR is the basal metabolic rate (numeric value) of a person given Gender, Weight, Height, and Age
bmr(male, Weight, Height, Age, BMR) :-
    W is Weight*10,
    H is Height*6.25,
    A is Age*5,
    BMR is W+H-A+5.
bmr(female, Weight, Height, Age, BMR) :-
    W is Weight*10,
    H is Height*6.25,
    A is Age*5,
    BMR is W+H-A-161.

% Activity is/or: 
% sedentary (little to no exercise)
% light (exercise 1-3 times per week)
% moderate (exercise 4-5 times per week)
% active (daily exercise or intense exercise 3-4 times per week)
% very (intense exercise 6-7 times per week)
% extra (very intense exercise daily or physical job)

%cal(BMR, Activity, Calories) is true if Calories is the required calories (numeric value) to maintain current weight given BMR and Activity
cal(BMR, Activity, Calories) :-
    activityLevel(Activity, F),
    Calories is BMR*F.

%activityLevel(Activity, Factor) is true Factor is the multiplier for calculating calories given Activity
activityLevel(sedentary, 1.20).
activityLevel(light, 1.37).
activityLevel(moderate, 1.46).
activityLevel(active, 1.55). 
activityLevel(very, 1.72).
activityLevel(extra, 1.90).

% Goal is/or:
% maintain (maintain weight)
% mildLoss (mild weight loss of about 0.25 kg per week)
% moderateLoss (moderate weight loss of about 0.5 kg per week)
% extremeWeightLoss (extreme weight loss of about 1 kg per week)
% mildGain (mild weight gain of about 0.25 kg per week)
% moderateGain (moderate weight gain of about 0.5 kg per week)
% fastGain (fast weight gain of about 1 kg per week)

%goalLevel(Goal, Factor) is true if Factor is the multiplier for calculating calories given Goal
goalLevel(maintain, 1.00).
goalLevel(mildLoss, 0.90).
goalLevel(moderateLoss, 0.80).
goalLevel(extremeWeightLoss, 0.60).
goalLevel(mildGain, 1.10).
goalLevel(moderateGain, 1.20).
goalLevel(fastGain, 1.40).

%calObjective(BMR, Activity, Goal, Objective) is true if the Objective is the required calories (numeric value) to alter weight given BMR, Activity, and Goal
calObjective(BMR, Activity, Goal, Objective) :-
    activityLevel(Activity, F),
    X is BMR*F,
    goalLevel(Goal, Y),
    Objective is X*Y.







