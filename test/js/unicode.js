// In the following lines, 'xyz' should be highlighted and nothing else.
xyz;
/* a */ xyz; // one byte UTF-8
/* ¡ */ xyz; // two byte UTF-8
/* ☃ */ xyz; // three byte UTF-8
/* ☃️ */ xyz; // variation selector
/* Ａ */ xyz; // doublewidth
/* ﾝ */ xyz; // halfwidth
/* à */ xyz; // combining
/* aa͜a */ xyz; // combining
/* 👩‍🎤 */ xyz; // ZWJ

let xyz;
