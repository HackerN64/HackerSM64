// Parameters: dialog enum ID, voice sound, lines per box, left offset, width

DEFINE_DIALOG(DIALOG_000, NO_SOUND, 6, 30, 200, "\
Du bist inmitten der\n\
Kampfarena gelandet.\n\
Sei vorsichtig!\n\
Die von Bowser erbeuteten\n\
Power-Sterne findest Du\n\
in den Wandgemälden.\n\
Sprich zuerst mit Buddy,\n\
der rosa Bombe.\n\
Drücke Ⓑ, um Dich mit ihr\n\
zu unterhalten. Sie und\n\
ihre Kolleginnen werden\n\
Dich sicher unterstützen.\n\
Drücke Ⓑ, um Schilder\n\
zu lesen. Mit Ⓐ oder Ⓑ\n\
kannst Du die Nachrichten\n\
umblättern. In anderen\n\
Kursen triffst Du weitere\n\
Freunde, die Dir helfen.")

DEFINE_DIALOG(DIALOG_001, SOUND_OBJ_BOBOMB_BUDDY_TALK, 4, 95, 200, "\
Bewege Dich vorsichtig\n\
durch das Gelände, damit\n\
Du kein Opfer der\n\
Wasserbomben wirst.\n\
Die gegnerischen Bob-\n\
Ombs lieben den Kampf\n\
und erfinden immer neue\n\
Angriffsvarianten.\n\
Sie terrorisieren uns,\n\
seit ihr König den\n\
Power-Stern in die\n\
Hände bekommen hat.\n\
Hilf uns, den Stern\n\
zurückzuholen! Gehe zur\n\
Spitze des Berges, um\n\
König Bob-Omb zu finden.\n\
Kehre zu mir zurück,\n\
wenn es Dir gelungen ist,\n\
ihm den Stern abzujagen.\n\
Viel Glück...")

DEFINE_DIALOG(DIALOG_002, SOUND_OBJ_BOBOMB_BUDDY_TALK, 4, 95, 200, "\
Hallo! Sei wachsam, Du\n\
befindest Dich inmitten\n\
einer Schlacht. Aber ich\n\
gebe Dir ein paar Tips:\n\
Überquere die beiden\n\
Brücken und achte auf\n\
herunterfallende\n\
Wasserbomben.\n\
König Bob-Omb ist\n\
äußerst gefährlich.\n\
Laß Dich nicht von\n\
ihm erwischen.\n\
Wir sind die wahren\n\
Bob-Ombs und werden Dir\n\
helfen. Sprich mit uns,\n\
wann immer Du möchtest.")

DEFINE_DIALOG(DIALOG_003, SOUND_OBJ_BOBOMB_BUDDY_TALK, 5, 95, 200, "\
Herzlichen Dank, Mario!\n\
Du hast es diesem\n\
Tyrannen gezeigt. Aber\n\
Dein Kampf hat gerade\n\
erst begonnen.\n\
Andere Fieslinge besitzen\n\
weitere Sterne, die Dir\n\
die Wege zu neuen Welten\n\
öffnen. Allerdings mußt\n\
Du sie zuerst besiegen.\n\
Meine Bob-Omb-Kollegen\n\
wissen bereits Bescheid.\n\
Sprich mit ihnen und\n\
Du darfst bestimmt ihre\n\
Kanonen benutzen.")

DEFINE_DIALOG(DIALOG_004, SOUND_OBJ_BOBOMB_BUDDY_TALK, 3, 95, 200, "\
Wir sind friedliebende\n\
Bob-Ombs und mögen keine\n\
Kanonen.\n\
Wir stellen sie Dir aber\n\
gerne für Luftreisen\n\
zur Verfügung.\n\
Alle Kanonen dieses Kurses\n\
sind für Dich präpariert.\n\
Guten Flug!!!")

DEFINE_DIALOG(DIALOG_005, SOUND_OBJ_KOOPA_TALK, 4, 30, 200, "\
Hey Mario! Stimmt es,\n\
daß Du König Bob-Omb\n\
während eines harten\n\
Kampfes besiegt hast?\n\
Du scheinst ganz schön\n\
was auf dem Kasten zu\n\
haben. Aber glaubst Du,\n\
es reicht auch für mich?\n\
Bist Du schnell genug, um\n\
mich zu schlagen? Ich\n\
würde sagen, Du siehst\n\
nicht danach aus!\n\
Wie wär's mit einem\n\
Rennen zur Bergspitze,\n\
um herauszufinden, wer\n\
der Schnellere ist?\n\
\n\
Fertig...?\n\
\n\
\tLos!\t   Später!")

DEFINE_DIALOG(DIALOG_006, SOUND_OBJ_KOOPA_TALK, 3, 30, 200, "\
Hey!!! Willst Du mich\n\
auf den Arm nehmen?\n\
Abkürzen gilt nicht!\n\
Am besten versuchst Du's\n\
später nochmal unter\n\
fairen Bedingungen.")

DEFINE_DIALOG(DIALOG_007, SOUND_OBJ_KOOPA_TALK, 5, 30, 200, "\
Hmmmmpff...pffff...hach!\n\
Boah! Du...hast...mich...\n\
geschlagen! Das grenzt an\n\
ein Wunder! Hier, Du hast\n\
ihn Dir verdient!")

DEFINE_DIALOG(DIALOG_008, NO_SOUND, 5, 30, 200, "\
BISSIGER KETTENHUND!\n\
Nähere Dich ihm langsam,\n\
und benutze die Ⓒ-Knöpfe,\n\
um den Blickwinkel zu\n\
ändern. Gefährlich, oder?\n\
Siehst Du die rote Münze\n\
auf dem Pfahl?\n\
Du erhältst einen Stern,\n\
wenn Du acht dieser\n\
Münzen sammelst.")

DEFINE_DIALOG(DIALOG_009, SOUND_OBJ_KOOPA_TALK, 5, 30, 200, "\
Lang, lang ist's her!\n\
Du scheinst noch schneller\n\
geworden zu sein. Hast Du\n\
heimlich geübt oder\n\
liegt's an den Sternen?\n\
Meine letzte Niederlage\n\
geht mir nicht aus dem\n\
Kopf. Aber das ist meine\n\
Hausstrecke. Wie wär's\n\
mit einer Revanche?\n\
Das Ziel liegt hinter\n\
der Böen-Brücke.\n\
Fertig?\n\
\n\
\tLos!\t   Später!")

/**
 * Special case: if the voice is SEQ_EVENT_SOLVE_PUZZLE,
 * then play_dialog_sound will run play_puzzle_jingle
 * instead of playing a sound effect
 */
DEFINE_DIALOG(DIALOG_010, SEQ_EVENT_SOLVE_PUZZLE, 4, 30, 200, "\
Du hast den roten\n\
Schalter aktiviert.\n\
Ab jetzt kannst Du die\n\
Federkappe einsetzen\n\
und durch die Lüfte\n\
fliegen. Jeder rote\n\
Block beinhaltet eine\n\
dieser Mützen.\n\
Möchtest Du Deinen\n\
Spielstand speichern?\n\
\n\
\tJa!\t\tNein!")

DEFINE_DIALOG(DIALOG_011, SEQ_EVENT_SOLVE_PUZZLE, 4, 30, 200, "\
Du hast den grünen\n\
Schalter aktiviert.\n\
Ab jetzt kannst Du die\n\
Titanenkappe einsetzen\n\
und Unbesiegbarkeit\n\
erlangen. Jeder grüne\n\
Block beinhaltet eine\n\
dieser Mützen.\n\
Möchtest Du Deinen\n\
Spielstand speichern?\n\
\n\
\tJa!\t\tNein!")

DEFINE_DIALOG(DIALOG_012, SEQ_EVENT_SOLVE_PUZZLE, 4, 30, 200, "\
Du hast den blauen\n\
Schalter aktiviert.\n\
Ab jetzt kannst Du die\n\
Tarnkappe einsetzen\n\
und Unsichtbarkeit\n\
erlangen. Jeder blaue\n\
Block beinhaltet eine\n\
dieser Mützen.\n\
Möchtest Du Deinen\n\
Spielstand speichern?\n\
\n\
\tJa!\t\tNein!")

DEFINE_DIALOG(DIALOG_013, NO_SOUND, 6, 30, 200, "\
Du hast 100 Münzen\n\
eingesammelt. Der\n\
Stern verleiht Dir\n\
zusätzliche Kraft.\n\
Spielstand speichern?\n\
\tJa!\t\tNein!")

DEFINE_DIALOG(DIALOG_014, NO_SOUND, 6, 30, 200, "\
Unglaublich, Du hast einen\n\
weiteren Stern erhalten.\n\
Er verleiht Dir\n\
zusätzliche Kraft.\n\
Spielstand speichern?\n\
\tJa!\t\tNein!")

DEFINE_DIALOG(DIALOG_015, NO_SOUND, 4, 30, 200, "\
Setze Deine Fäuste ein,\n\
um Dich zu verteidigen.\n\
Drücke Ⓐ zum Springen\n\
und Ⓑ zum Schlagen.\n\
Drücke Ⓐ und Ⓑ, um einen\n\
Tritt auszuführen.\n\
Gegenstände kannst Du\n\
mit Ⓑ nehmen und werfen.")

DEFINE_DIALOG(DIALOG_016, NO_SOUND, 4, 30, 200, "\
Springe auf den\n\
funkelnden Panzer!\n\
Der Krötensurf eröffnet\n\
ungeahnte Möglichkeiten!")

DEFINE_DIALOG(DIALOG_017, SOUND_OBJ_KING_BOBOMB_TALK, 4, 30, 200, "\
Ich bin König Bob-Omb,\n\
Herrscher über alle\n\
Bomben und Gebieter der\n\
Explosionen!\n\
Wer gibt Dir das Recht,\n\
diesen Berg zu betreten\n\
und dieses königliche\n\
Plateau zu beschmutzen?\n\
Anscheinend haben Dich\n\
die Wachen unterschätzt,\n\
aber mir wird dieser\n\
Fehler nicht unterlaufen.\n\
Der Power-Stern ist in\n\
meinem Besitz und wird\n\
auch dort bleiben.\n\
Dein Ende ist gekommen!\n\
Oder bist Du etwa der\n\
Meinung, Du könntest\n\
mich von hinten packen\n\
und besiegen? Niemals!!!")

DEFINE_DIALOG(DIALOG_018, NO_SOUND, 4, 30, 200, "\
Schlafende Gefahren\n\
sollte man nicht wecken.\n\
Die schleichende Weisheit\n\
verleiht längeres Leben!")

DEFINE_DIALOG(DIALOG_019, NO_SOUND, 2, 30, 200, "\
Vorsicht, die Fliesen\n\
sind frisch gebohnert!")

DEFINE_DIALOG(DIALOG_020, NO_SOUND, 6, 95, 150, "\
Lieber Mario!\n\
Komm mich doch einmal\n\
im Schloss besuchen!\n\
Der Kuchen steht bereit!\n\
In Freundschaft\n\
Toadstool")

DEFINE_DIALOG(DIALOG_021, SOUND_OBJ_BOWSER_INTRO_LAUGH, 5, 95, 200, "\
Na, wen haben wir denn\n\
hier? Es ist niemand zu\n\
Hause, also verschwinde\n\
besser wieder...\n\
Hua...hua...hua!!!")

DEFINE_DIALOG(DIALOG_022, SOUND_OBJ_BOWSER_INTRO_LAUGH, 3, 95, 200, "\
Ohne den passenden\n\
Schlüssel bleibt Dir\n\
dieser Weg versperrt!")

DEFINE_DIALOG(DIALOG_023, SOUND_OBJ_BOWSER_INTRO_LAUGH, 2, 95, 200, "\
Der Kellerschlüssel wird\n\
Dir hier nichts nützen!")

DEFINE_DIALOG(DIALOG_024, SOUND_OBJ_BOWSER_INTRO_LAUGH, 4, 95, 200, "\
Du brauchst schon einen\n\
Stern, um diese Tür zu\n\
öffnen. Untersuche doch\n\
einmal die Wandgemälde!")

DEFINE_DIALOG(DIALOG_025, SOUND_OBJ_BOWSER_INTRO_LAUGH, 4, 95, 200, "\
Zum Öffnen dieser Tür\n\
benötigst Du drei Sterne.\n\
Du mußt also noch %d\n\
von ihnen finden!")

DEFINE_DIALOG(DIALOG_026, SOUND_OBJ_BOWSER_INTRO_LAUGH, 4, 95, 200, "\
Zum Öffnen dieser Tür\n\
benötigst Du acht Sterne.\n\
Du mußt also noch %d\n\
von ihnen finden!")

DEFINE_DIALOG(DIALOG_027, SOUND_OBJ_BOWSER_INTRO_LAUGH, 4, 95, 200, "\
Zum Öffnen dieser Tür\n\
benötigst Du 30 Sterne.\n\
Du mußt also noch %d\n\
von ihnen finden.")

DEFINE_DIALOG(DIALOG_028, SOUND_OBJ_BOWSER_INTRO_LAUGH, 4, 95, 200, "\
Zum Öffnen dieser Tür\n\
benötigst Du 50 Sterne.\n\
Du mußt also noch %d\n\
von ihnen finden.")

DEFINE_DIALOG(DIALOG_029, SOUND_OBJ_BOWSER_INTRO_LAUGH, 4, 95, 200, "\
Zum Öffnen der Tür zum\n\
„Endlosen Vergnügen”\n\
benötigst Du 70 Sterne.\n\
Hua...Hua...Hua...!")

DEFINE_DIALOG(DIALOG_030, NO_SOUND, 4, 30, 200, "\
Die Lakitu-Film AG\n\
berichtet live und in\n\
Farbe! Hier die neuesten\n\
Meldungen:\n\
Gegner sind sehr oft\n\
unachtsam, wenn Du Dich\n\
leise, also langsam,\n\
bewegst.\n\
Die Kameraposition kannst\n\
Du durch Ⓒ▶ und Ⓒ◀\n\
verändern, um einen\n\
Überblick zu erhalten.\n\
Das Weitwinkelobjektiv\n\
wird mit Ⓒ▼ eingesetzt.\n\
Details der Umgebung\n\
werden so sichtbar!\n\
Ein Warnton erklingt,\n\
wenn die Bewegungs-\n\
freiheit der Kamera\n\
eingeschränkt ist.\n\
Das waren die\n\
Meldungen und\n\
damit zurück zum\n\
laufenden Programm!")

DEFINE_DIALOG(DIALOG_031, NO_SOUND, 3, 30, 200, "\
Das darf doch alles nicht\n\
wahr sein! Ich habe schon\n\
wieder verloren!\n\
Dabei habe ich mir extra\n\
die neuen Koopa-Mach-1-\n\
Schuhe gekauft! Hmpf!!!\n\
Naja, ich denke, auch\n\
dieser Stern gehört Dir!\n\
Herzlichen Glückwunsch!")

DEFINE_DIALOG(DIALOG_032, NO_SOUND, 5, 30, 200, "\
Mit der Federkappe\n\
kannst Du fliegen.\n\
Besitzt Du sie, springe\n\
dreimal hintereinander,\n\
um einen Flug zu starten.\n\
Benutzt Du zum Starten\n\
des Fluges eine Kanone,\n\
kannst Du die Flughöhe\n\
enorm steigern. Mit dem\n\
Ⓩ-Knopf kannst Du landen.")

DEFINE_DIALOG(DIALOG_033, NO_SOUND, 4, 30, 200, "\
Herzlich willkommen! Durch\n\
eine Warpröhre bist Du\n\
direkt zum Schloss der\n\
Prinzessin gelangt.\n\
Die Steuerung ist einfach:\n\
Benutze den Ⓐ-Knopf zum\n\
Springen und den Ⓑ-Knopf\n\
zum Schlagen.\n\
Hinweisschilder kannst\n\
Du lesen, wenn Du Dich\n\
davor stellst und den\n\
Ⓑ-Knopf betätigst.\n\
Der Analog-Stick dient\n\
der Steuerung. Doch jetzt\n\
genug der vielen Worte:\n\
Auf zum Schloss!!!")

DEFINE_DIALOG(DIALOG_034, NO_SOUND, 5, 30, 200, "\
Guten Tag, liebe Freunde\n\
des Actionkinos! Die\n\
Lakitu-Film AG versorgt\n\
Euch ständig mit den\n\
neuesten Informationen.\n\
Mario hat gerade\n\
Prinzessin Toadstools\n\
Schloss erreicht und\n\
macht sich auf die Suche\n\
nach den Power-Sternen.\n\
Unser wagemutiger Kollege\n\
wird ihn auf dieser\n\
heiklen Mission begleiten.\n\
Die Ⓒ-Knöpfe dienen\n\
seiner Steuerung.\n\
Du kannst die Perspektiven\n\
beliebig verändern. Sollte\n\
sich der Blickwinkel einmal\n\
nicht verstellen lassen,\n\
erklingt ein Warnton.\n\
Genauere Erklärungen\n\
hierzu werden wir Euch\n\
zu gegebener Zeit\n\
mitteilen. Damit zurück\n\
zum Hauptfilm!")

DEFINE_DIALOG(DIALOG_035, NO_SOUND, 5, 30, 200, "\
Die Ⓒ-Knöpfe dienen der\n\
Steuerung der Kamera.\n\
Benutze den ▲-Knopf, um\n\
die Kamera mit dem\n\
Analog-Stick zu bewegen.\n\
Normalerweise sorgt ein\n\
Angestellter der Lakitu\n\
AG dafür, daß Du Mario\n\
siehst. Das ist die\n\
Standardeinstellung.\n\
Diese Kamera steuerst Du\n\
mit den Ⓒ-Knöpfen.\n\
Drückst Du die Ⓡ-Taste,\n\
wechselst Du zwischen\n\
Lakitus und Marios\n\
Sicht hin und her. Drücke\n\
▼, um den Zoom oder\n\
das Weitwinkelobjektiv zu\n\
aktivieren. Dies ist in\n\
allen Perspektiven möglich.\n\
Welchen Kameramodus\n\
Du gewählt hast, siehst\n\
Du an einem kleinen Bild,\n\
das sich rechts unten auf\n\
dem Bildschirm befindet.")

DEFINE_DIALOG(DIALOG_036, NO_SOUND, 5, 30, 200, "\
AUSSICHTSPLATTFORM\n\
Drücke ▲ und genieße die\n\
Aussicht. Vielleicht\n\
entdeckst Du Geheimnisse\n\
Deiner Umgebung.\n\
Drücke Ⓡ, um zu Marios\n\
Kamera zu wechseln. Sie\n\
bleibt ständig hinter ihm.\n\
Mit dieser Taste schaltest\n\
Du auch zurück auf Lakitu.\n\
Im Pausenmodus kannst Du\n\
weitere Einstellungen\n\
vornehmen. Wähle „Stativ”\n\
und halte die Ⓡ-Taste\n\
gedrückt!")

DEFINE_DIALOG(DIALOG_037, SOUND_OBJ_BIG_PENGUIN_YELL, 3, 30, 200, "\
Hihihi, ich habe gewonnen!\n\
Du solltest noch ein paar\n\
Trainingsrunden einlegen!")

DEFINE_DIALOG(DIALOG_038, NO_SOUND, 2, 95, 200, "\
Die mystische Kraft der\n\
Sterne öffnet die Tür!")

DEFINE_DIALOG(DIALOG_039, NO_SOUND, 5, 30, 200, "\
Besuchern ist das\n\
Erklimmen des königlichen\n\
Berges strengstens\n\
verboten. Verstöße werden\n\
hart bestraft!\n\
Niemals werden die\n\
Power-Sterne, ein\n\
Geschenk Bowsers, diesen\n\
Ort verlassen. Ihre Macht\n\
bleibt in meinen Händen!\n\
Kein Wort über ihren\n\
Aufenthaltsort kommt\n\
über meine Lippen!!!\n\
Äh, naja, vielleicht\n\
ein kleiner Tip:\n\
Die Sternenbeschreibungen\n\
zu Beginn dieses Kurses\n\
könnten sich als nützlich\n\
erweisen!\n\
König Bob-Omb!!!")

DEFINE_DIALOG(DIALOG_040, NO_SOUND, 3, 30, 200, "\
Vorsicht, Brücke zerstört!\n\
Überquere das Eistal in\n\
der Gondel.")

DEFINE_DIALOG(DIALOG_041, SOUND_OBJ_KOOPA_TALK, 4, 30, 200, "\
Höhöhö, das war ja wohl\n\
nichts! Sogar meine Uroma\n\
Koopa ist wesentlich\n\
schneller als Du!\n\
Naja, mit zwei Jahren\n\
Training könntest Du's\n\
vielleicht schaffen.\n\
Bis dann...und tschüß!")

DEFINE_DIALOG(DIALOG_042, NO_SOUND, 4, 30, 200, "\
Achtung!\n\
Dieser Steg ist ziemlich\n\
schmal. Du solltest Dich\n\
sehr langsam bewegen.\n\
Überschreitest Du eine\n\
Kante, fällt Mario nicht\n\
in die Tiefe, sondern er\n\
klammert sich daran fest.\n\
Möchtest Du wieder nach\n\
oben klettern, drücke\n\
den Analog-Stick in\n\
Marios Blickrichtung.\n\
Mario läßt die Kante los,\n\
wenn Du den Analog-Stick\n\
in Richtung seines Rückens\n\
bewegst oder Ⓩ drückst.\n\
Bist Du in Eile, kannst\n\
Du auch durch einen\n\
beherzten Sprung mit dem\n\
Ⓐ-Knopf hinauf gelangen.")

DEFINE_DIALOG(DIALOG_043, NO_SOUND, 4, 30, 200, "\
Wenn Du springst und den\n\
Ⓐ-Knopf gedrückt hältst,\n\
klammert sich Mario an\n\
Objekte über ihm.\n\
Auf diese Weise kannst\n\
Du Dich auch von der\n\
Eule durch die Lüfte\n\
transportieren lassen.")

DEFINE_DIALOG(DIALOG_044, NO_SOUND, 5, 95, 200, "\
Uh...oh...gääähn...\n\
Weeehhr ist daaaahhh...?\n\
Wer hat mich geweckt?\n\
Eigentlich sollte ich um\n\
diese Tageszeit schlafen!\n\
Aber, da Du mich schon\n\
geweckt hast, wie wäre\n\
es dann mit einem\n\
kleinen Rundflug um die\n\
Burg?\n\
Befinde ich mich direkt\n\
über Dir, springe und\n\
halte den Sprungknopf\n\
gedrückt. Laß ihn los,\n\
um wieder frei zu sein.\n\
Ich transportiere Dich,\n\
solange es meine Kraft\n\
zuläßt. Beobachte meinen\n\
Schatten, um Dich zu\n\
orientieren.")

DEFINE_DIALOG(DIALOG_045, NO_SOUND, 6, 95, 200, "\
Hey, Mann, Mario! Mir\n\
geht die Puste aus.\n\
Du ißt zuviel Pasta!\n\
Bitte, laß los, ich muß\n\
mich erholen...sofooort!\n\
Bis später...vielleicht.")

DEFINE_DIALOG(DIALOG_046, NO_SOUND, 4, 30, 200, "\
Es gibt drei verschiedene\n\
Sprungtechniken. Du mußt\n\
sie alle beherrschen, um\n\
die Aufgaben zu meistern.\n\
Versuche zuerst den\n\
Dreisprung! Renne und\n\
springe dreimal direkt\n\
hintereinander.\n\
Mit dem richtigen Timing\n\
wird jeder Sprung höher\n\
als der vorherige sein.\n\
Weiter zum Weitsprung!\n\
Während Du rennst, mußt\n\
Du den Ⓩ-Knopf und dann\n\
den Ⓐ-Knopf drücken.\n\
Und nun der Wandsprung!\n\
Springe gegen eine Wand.\n\
Sobald Du die Wand\n\
berührst, mußt Du wieder\n\
den Sprungknopf drücken.\n\
Alles verstanden?\n\
Dreisprung? Weitsprung?\n\
Wandsprung? Dann heißt's\n\
üben, üben, üben...")

DEFINE_DIALOG(DIALOG_047, SOUND_OBJ_BOBOMB_BUDDY_TALK, 3, 95, 200, "\
Hallo!\n\
Ich mache die Kanone\n\
zum Abschuß bereit!")

DEFINE_DIALOG(DIALOG_048, NO_SOUND, 6, 30, 200, "\
An einigen Stellen\n\
herrscht extreme\n\
Rutschgefahr! Sieh\n\
am besten zuerst im\n\
Schornstein nach dem\n\
Rechten!")

DEFINE_DIALOG(DIALOG_049, NO_SOUND, 5, 30, 200, "\
Du erinnerst Dich an den\n\
Wandsprung? Auf diese\n\
Weise kannst Du schnell\n\
höher gelegene Stellen\n\
erreichen.\n\
Benutze ihn, um von\n\
Wand zu Wand zu springen\n\
und Stück für Stück\n\
weiter nach oben zu\n\
gelangen.\n\
Denke an die uralte\n\
Weisheit:\n\
Übung macht den Meister!\n\
Sie kommt hier voll zum\n\
Tragen!!!")

DEFINE_DIALOG(DIALOG_050, NO_SOUND, 4, 30, 200, "\
Drücke den Ⓩ-Knopf, um\n\
in die Hocke zu gehen und\n\
einen Hang nach unten zu\n\
rutschen.\n\
Betätigst Du den Ⓩ-Knopf\n\
während eines Sprungs,\n\
führst Du eine\n\
Stampfattacke aus.\n\
Du machst einen\n\
Rückwärtssalto, wenn\n\
Du stehend den Ⓩ-Knopf\n\
drückst und springst.\n\
Es gibt weitere Varianten\n\
zu entdecken. Nimm Dir\n\
Zeit und versuche, andere\n\
Kombinationen zu finden!")

DEFINE_DIALOG(DIALOG_051, NO_SOUND, 5, 30, 200, "\
Du kannst auf Bäume und\n\
Stangen klettern, wenn\n\
Du gegen sie springst und\n\
den Analog-Stick nach\n\
oben drückst.\n\
Mit dem Ⓐ-Knopf kannst\n\
Du nach HINTEN wieder\n\
abspringen. Du kannst\n\
sogar auf der Spitze\n\
einen Handstand machen.\n\
Springst Du aus dem\n\
Handstand von einem\n\
Objekt ab, machst Du\n\
einen unglaublich hohen\n\
Sprung.")

DEFINE_DIALOG(DIALOG_052, NO_SOUND, 5, 30, 200, "\
Du machst einen\n\
Rückwärtssalto, wenn\n\
Du stehend den Ⓩ-Knopf\n\
gedrückt hältst und dann\n\
den Ⓐ-Knopf betätigst.\n\
Seitwärtssaltos führst\n\
Du aus, indem Du den\n\
Analog-Stick entgegen\n\
Deiner Laufrichtung\n\
bewegst und springst.")

DEFINE_DIALOG(DIALOG_053, NO_SOUND, 5, 30, 200, "\
Von Zeit zu Zeit erscheint\n\
eine farbige Zahl, wenn\n\
Du Kisten öffnest, Ringe\n\
durchquerst oder geheime\n\
Orte erreichst.\n\
Gelingt es Dir, alle fünf\n\
farbigen Zahlen dieses\n\
Bereichs zu finden,\n\
erhältst Du zur Belohnung\n\
einen Power-Stern.")

DEFINE_DIALOG(DIALOG_054, NO_SOUND, 6, 30, 200, "\
Herzlich willkommen\n\
auf der Schlidderbahn!\n\
Drücke den Analog-Stick\n\
nach vorne, um zu\n\
beschleunigen und nach\n\
hinten, um zu bremsen.")

DEFINE_DIALOG(DIALOG_055, SOUND_OBJ_BIG_PENGUIN_YELL, 5, 30, 200, "\
Hihi, hallo Mario!\n\
Du siehst aus, als\n\
wolltest Du mich gerade\n\
fragen, ob wir ein\n\
Wettrennen machen sollten.\n\
Und ich sage: Na klar! Es\n\
ist zwar noch niemandem\n\
gelungen, mich, den\n\
Schlidderkönig aller\n\
Klassen, zu besiegen.\n\
Aber Du kannst es ja\n\
einmal versuchen.\n\
Wie steht's?\n\
\n\
\tNa klar!   Später!")

DEFINE_DIALOG(DIALOG_056, SOUND_OBJ_BIG_PENGUIN_YELL, 6, 30, 200, "\
Du...Du...ha...hast\n\
mich geschlagen! Das kann\n\
doch nicht wahr sein!\n\
Du bist der größte\n\
Schliddermeister, den\n\
ich kenne!\n\
Eine Goldmedaille kann\n\
ich Dir zwar nicht\n\
überreichen,\n\
aber nimm diesen\n\
Power-Stern!\n\
Du hast ihn verdient.")

DEFINE_DIALOG(DIALOG_057, SOUND_OBJ_BIG_PENGUIN_YELL, 4, 30, 200, "\
Hey Kleiner! Hast Du mein\n\
Baby gesehen? Es ist das\n\
schönste und süßeste\n\
Baby der ganzen Welt!\n\
Vor kurzem war es noch\n\
bei mir, aber ich habe\n\
keine Ahnung, wohin es\n\
sich verdrückt hat.\n\
Wenn ich doch nur wüßte,\n\
wo ich die Suche beginnen\n\
soll...Oh, was für eine\n\
fürchterliche Tragödie!!!")

DEFINE_DIALOG(DIALOG_058, SOUND_OBJ_BIG_PENGUIN_YELL, 5, 30, 200, "\
Das ist doch...\n\
Mensch, Mario, Du hast\n\
mein Baby gefunden!!!\n\
Wie kann ich Dir dafür\n\
danken?\n\
Oh, ich weiß: Ich fand\n\
neulich diesen Stern\n\
hier. Nimm ihn als\n\
Zeichen meiner ewigen\n\
Dankbarkeit!")

DEFINE_DIALOG(DIALOG_059, SOUND_OBJ_BIG_PENGUIN_YELL, 6, 30, 200, "\
Was soll ich mit diesem\n\
Früchtchen? Das ist nicht\n\
mein Baby! Mein Baby hat\n\
ein weiche, zarte Stimme,\n\
und es sieht mir natürlich\n\
ähnlich!")

DEFINE_DIALOG(DIALOG_060, NO_SOUND, 4, 30, 200, "\
Achtung, Achtung!\n\
Bevor Du Dich in die\n\
Fluten stürzt, solltest\n\
Du Dir folgendes merken:\n\
Bleibst Du zu lange unter\n\
Wasser, könnte Dir die\n\
Luft ausgehen. Achte auf\n\
die Sauerstoffanzeige!\n\
Schwimme zur Oberfläche\n\
oder sammle Münzen und\n\
Luftblasen, um wieder\n\
Sauerstoff zu tanken.\n\
Drücke den Ⓐ-Knopf, um\n\
zu schwimmen. Halte ihn\n\
gedrückt, um mit den\n\
Füßen zu paddeln.\n\
Bewege beim Schwimmen\n\
den Analog-Stick nach\n\
oben, um zu tauchen und\n\
nach unten, um zu steigen.\n\
Springe aus dem Wasser,\n\
indem Du an einer Kante\n\
den Analog-Stick abwärts\n\
drückst und springst.\n\
Achte jedoch darauf,\n\
daß Du den Analog-Stick\n\
während des Sprungs\n\
losläßt!")

DEFINE_DIALOG(DIALOG_061, NO_SOUND, 5, 30, 200, "\
Wer baden möchte, sollte\n\
sich besser in wärmere\n\
Gefilde begeben, denn\n\
hier ist es auf jeden\n\
Fall zu kalt!!!")

DEFINE_DIALOG(DIALOG_062, NO_SOUND, 4, 30, 200, "\
In den grünen Blöcken\n\
im Labyrinth findest\n\
Du die phänomenalen\n\
Titanenkappen.\n\
Trägst Du diese Kappen,\n\
bist Du unverwundbar\n\
und brauchst kurze Zeit\n\
nicht zu atmen.\n\
Das einzige Problem ist:\n\
Du kannst nicht\n\
schwimmen, während Du\n\
sie trägst!")

DEFINE_DIALOG(DIALOG_063, NO_SOUND, 3, 30, 200, "\
In den blauen Blöcken\n\
befinden sich die\n\
fantastischen Tarnkappen.\n\
Damit kannst Du durch\n\
bestimmte Mauern gehen\n\
oder Geister erschrecken.")

DEFINE_DIALOG(DIALOG_064, NO_SOUND, 3, 30, 200, "\
In den roten Blöcken\n\
kannst Du die berühmten\n\
Federkappen finden.\n\
Mit einem Dreisprung\n\
kannst Du starten und\n\
in die Lüfte steigen.\n\
Drücke den Analog-Stick\n\
nach unten, um im Flug\n\
an Höhe zu gewinnen.\n\
Drücke den Analog-Stick\n\
nach oben, um im Flug\n\
an Höhe zu verlieren.\n\
Betätige den Ⓩ-Knopf,\n\
um den Flug zu beenden\n\
und sicher zu landen.")

DEFINE_DIALOG(DIALOG_065, NO_SOUND, 5, 30, 200, "\
Schwimmen für Anfänger!\n\
Drücke den Ⓐ-Knopf für\n\
einen Schwimmzug. Bei\n\
richtigem Timing kannst\n\
Du sehr schnell schwimmen.\n\
Halte den Ⓐ-Knopf\n\
gedrückt, um mit den\n\
Füßen zu paddeln. Mit\n\
dieser Technik bewegst Du\n\
Dich langsamer im Wasser.\n\
Drücke beim Schwimmen\n\
den Analog-Stick\n\
nach oben, um zu\n\
tauchen und nach unten,\n\
um zu steigen.\n\
Du kannst aus dem Wasser\n\
springen, wenn Du an der\n\
Wasseroberfläche den\n\
Analog-Stick nach unten\n\
drückst und springst.\n\
Beachte: Kein Mensch\n\
kann unter Wasser atmen!\n\
Kehre zurück an die\n\
Oberfläche, wenn Deine\n\
Luft zur Neige geht.\n\
Ach ja: Türen, die sich\n\
unter Wasser befinden,\n\
kannst Du nicht öffnen.\n\
Aber vielleicht findest\n\
Du ja einen Abfluß!!!")

DEFINE_DIALOG(DIALOG_066, NO_SOUND, 5, 30, 200, "\
Hallo Mario, ich bin's,\n\
Peach! Paß auf Dich auf!\n\
Bowser wird versuchen,\n\
Dich mit seinem Feueratem\n\
zu versengen.\n\
Renne hinter ihn!\n\
Versuche mit dem\n\
Ⓑ-Knopf Bowsers\n\
Schwanz zu packen und\n\
ihn herumzuschleudern.\n\
Bewege den Analog-Stick\n\
im Kreis, um Dich zu\n\
drehen. Je schneller\n\
Du Dich drehst, desto\n\
weiter wird Dein Wurf.\n\
Benutze die Ⓒ-Knöpfe, um\n\
Dich zu orientieren. Du\n\
mußt Bowser gegen eine\n\
der Bomben am Rand der\n\
Plattform werfen.\n\
Drücke den Ⓑ-Knopf, um\n\
Bowser loszulassen und\n\
in Richtung der Bombe\n\
zu schleudern.\n\
Viel Glück!!!")

DEFINE_DIALOG(DIALOG_067, SOUND_OBJ_BOWSER_LAUGH, 5, 30, 200, "\
Tja, Mario, Pech gehabt!\n\
Die Prinzessin ist nicht\n\
hier und wird es auch so\n\
bald nicht sein...\n\
Hua...Hua...Hua...Hua!\n\
Es wird Dir niemals\n\
gelingen, mich von hinten\n\
zu packen und zu werfen.\n\
Da müßte schon ein wahrer\n\
Held kommen, keine Wurst!\n\
Vielleicht gehst Du besser\n\
wieder nach Hause und\n\
reparierst weiter kaputte\n\
Rohre, als Dich mit mir\n\
zu messen!!!")

DEFINE_DIALOG(DIALOG_068, NO_SOUND, 5, 30, 200, "\
Du bist im Land des\n\
flüssigen Feuers. Wenn\n\
Du vom Weg abkommst,\n\
bewahre Ruhe, denn Du\n\
verlierst nicht die\n\
gesamte Energie auf\n\
einmal. Außerdem kannst\n\
Du den qualmenden Mario\n\
während seines Höhenflugs\n\
immer noch steuern!")

DEFINE_DIALOG(DIALOG_069, NO_SOUND, 4, 30, 200, "\
Während Deiner Abenteuer\n\
in den Wandbildern stößt\n\
Du an den Rändern auf\n\
unsichtbare Mauern.\n\
Triffst Du fliegend auf\n\
eine solche Mauer, prallst\n\
Du ab. Du kannst aber den\n\
Flug fortsetzen.")

DEFINE_DIALOG(DIALOG_070, NO_SOUND, 4, 30, 200, "\
Du kannst die Wandbilder\n\
jederzeit verlassen, um\n\
in die Schlosshalle\n\
zurückzukehren.\n\
Bleibe stehen, drücke\n\
START, um das Spiel\n\
zu pausieren, und wähle\n\
„Kurs verlassen”!\n\
Du mußt nicht alle Sterne\n\
einer Welt finden, um\n\
den nächsten Abschnitt\n\
betreten zu können.\n\
Hebe Dir die schweren\n\
Brocken für später auf,\n\
wenn Du Deine Techniken\n\
perfektioniert hast.\n\
Findest Du einen Stern,\n\
erhältst Du einen Hinweis,\n\
wo sich der nächste\n\
Fundort befindet.\n\
Aber Du mußt die Sterne\n\
in keiner bestimmten\n\
Reihenfolge finden.\n\
Die Wahl liegt bei Dir!")

DEFINE_DIALOG(DIALOG_071, NO_SOUND, 4, 30, 200, "\
Achtung! Hüte Dich vor\n\
den Nebelschwaden!\n\
Sie enthalten keinen\n\
Sauerstoff!\n\
Benutze die erhöhten\n\
Plattformen, um Dich\n\
auszuruhen und einen\n\
sicheren Weg zu suchen.\n\
Auf der Karte sind\n\
Unterstände als Kreise\n\
dargestellt, den Eingang\n\
erkennst Du am Pfeil!")

DEFINE_DIALOG(DIALOG_072, NO_SOUND, 5, 30, 200, "\
Hier oben weht eine steife\n\
Brise. Solltest Du Deine\n\
Mütze verlieren, kannst\n\
Du sie auf dem Weg zum\n\
Gipfel wiederfinden.")

DEFINE_DIALOG(DIALOG_073, NO_SOUND, 5, 95, 200, "\
Ahoi, Landratte! Es ist\n\
doch wohl logisch, daß\n\
sich hier unten ein\n\
sagenhafter Schatz\n\
befindet!\n\
Zur Bergung mußt Du die\n\
Kisten in der richtigen\n\
Reihenfolge öffnen, klar?\n\
Aber ich sage nicht, wie\n\
sie lautet, hehehe!")

DEFINE_DIALOG(DIALOG_074, NO_SOUND, 5, 30, 200, "\
Siehst Du den Block\n\
nebenan? Manchmal kannst\n\
Du im Innern dieser\n\
Quader interessante\n\
Entdeckungen machen.\n\
Zerstöre sie mit einem\n\
gezielten Schlag oder\n\
einer Stampfattacke,\n\
damit sie ihr Geheimnis\n\
preisgeben!")

DEFINE_DIALOG(DIALOG_075, NO_SOUND, 5, 30, 200, "\
Hilfe, Mario! Mein Schloss\n\
befindet sich in großer\n\
Gefahr. Ich weiß, daß der\n\
widerliche Bowser seine\n\
Hände im Spiel hat.\n\
Er hat alle Türen des\n\
Schlosses versiegelt. Nur\n\
die magische Kraft der\n\
Power-Sterne kann diese\n\
Siegel brechen.\n\
Aber es gibt einige\n\
Geheimwege im Innern, die\n\
Bowser nicht entdeckt\n\
hat. Einer davon befindet\n\
sich in diesem Raum.\n\
Suche den Eingang und\n\
finde den Power-Stern,\n\
der sich darin verbirgt.\n\
Hilf uns, Du bist unsere\n\
einzige Hoffnung!\n\
Fast alle Power-Sterne\n\
kannst Du in den\n\
Wandgemälden finden,\n\
manche jedoch an\n\
geheimen Orten.\n\
Sammle soviele Sterne\n\
wie möglich, um Bowsers\n\
Plan zu vereiteln.\n\
Wir zählen auf Dich!\n\
Viel Glück!!!")

DEFINE_DIALOG(DIALOG_076, NO_SOUND, 4, 30, 200, "\
Hallo Mario! Du hast\n\
bereits einiges zur\n\
Rettung des Schlosses\n\
getan.\n\
Aber Du mußt noch\n\
viele Geheimnisse\n\
der Wandgemälde\n\
lüften.\n\
Manchmal spielen Dir\n\
Deine Augen einen Streich.\n\
Die Realität verschwindet\n\
hinter Fassaden.\n\
Aber Du kannst das\n\
Trugbild durchdringen,\n\
wenn Du es im Spiegel\n\
betrachtest!\n\
Wasser ist ein flüssiges\n\
Element. Die Magie des\n\
Wandgemäldes beeinflußt\n\
sein Verhalten.\n\
Wähle verschiedene Wege,\n\
um die versunkene Stadt\n\
durch das Gemälde zu\n\
betreten.\n\
Ach ja, fast hätte ich es\n\
vergessen: Hier habe ich\n\
etwas für Dich! Es hilft\n\
Dir, Bowser zu besiegen.")

DEFINE_DIALOG(DIALOG_077, NO_SOUND, 4, 150, 200, "\
Einst konnte man die Tür\n\
öffnen. Zu dieser Zeit\n\
waren die Säulen aber\n\
niedriger!")

DEFINE_DIALOG(DIALOG_078, NO_SOUND, 5, 30, 200, "\
Diesen Schalter kannst Du\n\
durch eine Stampfattacke\n\
auslösen. Er läßt einige\n\
blaue Münzen erscheinen,\n\
die je fünf gelbe\n\
Münzen wert sind.\n\
Allerdings mußt Du Dich\n\
beeilen, denn die blauen\n\
Münzen erscheinen nur\n\
für kurze Zeit.")

DEFINE_DIALOG(DIALOG_079, SOUND_OBJ_UKIKI_CHATTER_LONG, 4, 30, 200, "\
Auuuuuaaa! Hey, laß mich\n\
los! Das war doch nur\n\
Spaß! Du hast wohl keinen\n\
Humor, was?\n\
Okay, ich mache Dir einen\n\
Vorschlag: Wenn Du mich\n\
losläßt, habe ich eine\n\
Überraschung für Dich!\n\
Na, wie sieht's aus?\n\
\n\
\tKlar!\t  Vergiß es!")

DEFINE_DIALOG(DIALOG_080, SOUND_OBJ_UKIKI_CHATTER_LONG, 3, 30, 200, "\
Hey Baby, komm schon!\n\
Wo bleibst Du denn so\n\
lange?")

DEFINE_DIALOG(DIALOG_081, NO_SOUND, 5, 30, 200, "\
Des Rätsels Lösung ist der\n\
Wasserstand. Tief unten\n\
erwartet Dich die Stadt,\n\
nachdem Du den Pegel\n\
hast sinken lassen.")

DEFINE_DIALOG(DIALOG_082, NO_SOUND, 4, 30, 200, "\
Achte auf Deine Mütze!\n\
Solltest Du sie verlieren,\n\
wirst Du durch Treffer\n\
schwerer verletzt.\n\
Solltest Du sie einmal\n\
verlieren, findest Du die\n\
Mütze in dem Gemälde, in\n\
dem Du sie verloren hast.\n\
Die Prinzessin wird\n\
noch immer von Bowser\n\
gefangen gehalten. Rette\n\
sie so schnell wie möglich.\n\
Bowsers Schergen belagern\n\
seit langer Zeit die\n\
Welten der Gemälde und\n\
Wände des Schlosses.\n\
Aber es ist mir gelungen,\n\
ihnen diesen Stern\n\
abzujagen.\n\
Viel Glück!")

DEFINE_DIALOG(DIALOG_083, NO_SOUND, 6, 30, 200, "\
Betrittst Du die Uhr zu\n\
verschiedenen Zeiten,\n\
werden Dir weitere\n\
Geheimnisse offenbart.\n\
Vielleicht hilft Dir auch\n\
dieser Stern!")

DEFINE_DIALOG(DIALOG_084, NO_SOUND, 5, 30, 200, "\
Hey, Du Rüpel, ich\n\
bekomme ja blaue Flecken.\n\
Laß mich sofort los!\n\
Bowser hat mir diesen\n\
Stern geschenkt.\n\
Er wäre ziemlich böse,\n\
wenn er das wüßte, aber\n\
ich bin in Eile. Also\n\
nimm ihn und laß mich\n\
runter!")

DEFINE_DIALOG(DIALOG_085, SOUND_OBJ_BOO_LAUGH_LONG, 4, 30, 200, "\
Im Horrorhaus solltest Du\n\
den Mund geschlossen\n\
halten, damit Deine Zähne\n\
nicht vor Angst klappern!")

DEFINE_DIALOG(DIALOG_086, NO_SOUND, 3, 40, 200, "\
Wenn Du im Kreis rennst,\n\
werden manche Gegner\n\
große Augen machen!")

DEFINE_DIALOG(DIALOG_087, NO_SOUND, 3, 30, 200, "\
Der Weihnachtsmann ist\n\
nicht der einzige, der\n\
in Schornsteine klettert!")

DEFINE_DIALOG(DIALOG_088, NO_SOUND, 2, 30, 200, "\
Benutze die Stange, um\n\
nach unten zu gelangen!")

DEFINE_DIALOG(DIALOG_089, NO_SOUND, 4, 95, 200, "\
Auf beiden Wegen lauern\n\
Gefahren. Links benötigst\n\
Du den Weitsprung, um\n\
weiterzukommen.\n\
Rechts: Arbeitsplattform\n\
\t\tNebellabyrinth\n\
Links:  Schwarzes Loch\n\
\t\tHöhlensee")

DEFINE_DIALOG(DIALOG_090, SOUND_OBJ_BOWSER_LAUGH, 5, 30, 200, "\
Hua, hua, hua, ich wußte,\n\
daß Du mir in die Falle\n\
gehen würdest. Du solltest\n\
öfter auf Deine ulkigen\n\
Füße schauen!")

DEFINE_DIALOG(DIALOG_091, NO_SOUND, 3, 30, 200, "\
Achtung, starke Böen!\n\
Aber es könnte auch eine\n\
aufregende Reise werden!")

DEFINE_DIALOG(DIALOG_092, SOUND_OBJ_BOWSER_LAUGH, 4, 30, 200, "\
Was willst Du denn schon\n\
wieder? Du bist ja\n\
lästiger als ein Schwarm\n\
Stubenfliegen.\n\
Gerade jetzt, als mir\n\
die ganze Sache so viel\n\
Spaß machte! Aber, wenn\n\
Du schon mal hier bist:\n\
Gib mir die Power-Sterne\n\
zurück! Meine Truppen\n\
können sie besser\n\
gebrauchen als Du.")

DEFINE_DIALOG(DIALOG_093, SOUND_OBJ_BOWSER_LAUGH, 4, 30, 200, "\
Mario, wie schön Dich zu\n\
sehen! Ich dachte gerade\n\
an eine Grillparty - schon\n\
bist Du da.\n\
Jetzt brauche ich mir\n\
wenigstens keine Gedanken\n\
zu machen, was ich grille:\n\
Nämlich Dich!\n\
Dann gehört die Prinzessin\n\
endgültig mir und ich bin\n\
auf ewig der Herrscher\n\
des Schlosses!!!")

DEFINE_DIALOG(DIALOG_094, NO_SOUND, 4, 30, 200, "\
Erinnerst Du Dich noch an\n\
den Weitsprung? Renne,\n\
drücke den Ⓩ-Knopf und\n\
dann den Ⓐ-Knopf!")

DEFINE_DIALOG(DIALOG_095, NO_SOUND, 3, 30, 200, "\
Wie man Schilder liest,\n\
hast Du offensichtlich\n\
bereits herausgefunden.\n\
Auf die gleiche Weise\n\
kannst Du mit anderen\n\
sprechen.")

DEFINE_DIALOG(DIALOG_096, NO_SOUND, 4, 30, 200, "\
Der Weg zur Burg ist\n\
sehr schmal! Selbst\n\
Wagemutige sollten sich\n\
langsam bewegen.\n\
Außerdem wecken leise\n\
Schritte niemanden, der\n\
schläft - was Dein\n\
Vorteil sein könnte!")

DEFINE_DIALOG(DIALOG_097, NO_SOUND, 6, 30, 200, "\
Sei nicht schüchtern,\n\
sondern wehre Dich,\n\
wenn jemand versucht,\n\
Dich von einer Plattform\n\
zu schubsen. Sonst steigt\n\
die Temperatur!")

DEFINE_DIALOG(DIALOG_098, SOUND_OBJ_BOO_LAUGH_LONG, 1, 95, 200, "\
Komm nur näher, hehehe!")

DEFINE_DIALOG(DIALOG_099, SOUND_OBJ_BOO_LAUGH_LONG, 3, 95, 200, "\
")

DEFINE_DIALOG(DIALOG_100, SOUND_OBJ_UKIKI_CHATTER_LONG, 2, 95, 200, "\
Jippiiiiieee, ich hab' sie!\n\
Jetzt gehört sie mir!!!")

DEFINE_DIALOG(DIALOG_101, SOUND_OBJ_UKIKI_CHATTER_LONG, 5, 95, 200, "\
Hgggnnnhhh! Laß...mich...\n\
los!!! Diese Mütze? Na\n\
gut, ich gebe sie Dir,\n\
obwohl sie mir besser\n\
steht als Dir!")

DEFINE_DIALOG(DIALOG_102, NO_SOUND, 5, 30, 200, "\
Hey, pssst, paß mal auf:\n\
Die Geister sind sehr\n\
schüchtern. Siehst Du\n\
ihnen in die Augen,\n\
verschwinden sie.\n\
Drehst Du ihnen allerdings\n\
den Rücken zu, werden\n\
sie wieder sichtbar.\n\
Attackiere sie von hinten,\n\
um sie zu besiegen.")

DEFINE_DIALOG(DIALOG_103, NO_SOUND, 4, 95, 200, "\
Die Pyramide ist von vier\n\
Säulen umgeben. Erklimme\n\
die Spitzen der Säulen, um\n\
das Geheimnis zu lüften.")

DEFINE_DIALOG(DIALOG_104, NO_SOUND, 4, 30, 200, "\
Der Schattenstern vor Dir\n\
wird real, wenn Du die\n\
acht roten Münzen dieser\n\
Welt findest.")

DEFINE_DIALOG(DIALOG_105, SOUND_OBJ_BOBOMB_BUDDY_TALK, 5, 95, 200, "\
Bist Du bereit? Dann rein\n\
mit Dir in die Kanone! Du\n\
kannst mir ihrer Hilfe\n\
die schwebende Insel\n\
erreichen!\n\
Benutze den Analog-Stick\n\
zum Zielen und feuere die\n\
Kanone mit dem Ⓐ-Knopf\n\
ab. Akrobaten bevorzugen\n\
Bäume zur Landung!")

DEFINE_DIALOG(DIALOG_106, SOUND_OBJ_BOBOMB_BUDDY_TALK, 2, 95, 200, "\
Bist Du bereit? Dann rein\n\
mit Dir in die Kanone!")

DEFINE_DIALOG(DIALOG_107, SOUND_OBJ_BOO_LAUGH_LONG, 4, 95, 200, "\
Das werde ich meinem\n\
großen Bruder erzählen!\n\
Dann kannst Du aber was\n\
erleben...")

DEFINE_DIALOG(DIALOG_108, SOUND_OBJ_BOO_LAUGH_LONG, 6, 95, 200, "\
Ha-Boooo! Du bist also\n\
der Unruhestifter...\n\
Jetzt wirst Du das büßen,\n\
was Du mir und meinen\n\
Geschwistern angetan\n\
hast...")

DEFINE_DIALOG(DIALOG_109, NO_SOUND, 4, 95, 200, "\
Schnief, schnüff! Mein\n\
einst stattlicher Körper\n\
hat sich in Wasser\n\
aufgelöst.\n\
Ich würde alles für einen\n\
neuen Körper geben.\n\
Vielleicht kannst Du mir\n\
helfen, schluchz?")

DEFINE_DIALOG(DIALOG_110, NO_SOUND, 5, 95, 200, "\
Oh Mann, ich fühle mich\n\
so kopflos! Kennst Du\n\
jemanden, der nach einem\n\
Körper sucht? Unten?\n\
Okay, dann los!")

DEFINE_DIALOG(DIALOG_111, NO_SOUND, 3, 95, 200, "\
Boah, welch komfortables\n\
Unterteil!\n\
Ich bin so glücklich...\n\
Hier, nimm den Stern.\n\
Ich hoffe, er bringt\n\
Dir Glück!")

DEFINE_DIALOG(DIALOG_112, NO_SOUND, 4, 30, 200, "\
Sammle so viele Münzen,\n\
wie Du finden kannst.\n\
Sie geben Dir verlorene\n\
Energie zurück.\n\
Die Bestleistung jeder\n\
Welt wird gespeichert.\n\
Du kannst sie jederzeit\n\
abrufen.\n\
In einigen Welten wirst\n\
Du transparenten Herzen\n\
begegnen. Auch sie füllen\n\
Deinen Power-Meter auf.\n\
Je schneller Du das Herz\n\
passierst, desto mehr\n\
verlorene Energie wird\n\
ersetzt.")

DEFINE_DIALOG(DIALOG_113, NO_SOUND, 6, 30, 200, "\
In den roten, blauen und\n\
grünen Blöcken befinden\n\
sich verschiedene Mützen.\n\
Du mußt die versteckten\n\
Schalter finden, um die\n\
Blöcke öffnen zu können.")

DEFINE_DIALOG(DIALOG_114, SOUND_OBJ2_BOSS_DIALOG_GRUNT, 4, 95, 200, "\
Das gibt es doch gar\n\
nicht! Was willst Du\n\
Wicht hier in meinem\n\
Reich?\n\
Ich werde Dich lehren,\n\
meine Befehle zu...\n\
Oh, diese vermaledeiten\n\
Rückenschmerzen...!")

DEFINE_DIALOG(DIALOG_115, SOUND_OBJ2_BOSS_DIALOG_GRUNT, 4, 95, 200, "\
Unglaublich! Du hast mich\n\
von meinen Kreuzschmerzen\n\
befreit! Als Dank gebe\n\
ich Dir diesen Stern!")

DEFINE_DIALOG(DIALOG_116, SOUND_OBJ2_BOSS_DIALOG_GRUNT, 4, 95, 200, "\
Waaaaa...wie? Du hast\n\
mich besiegt...\n\
Ich verneige mein Haupt\n\
vor Deiner Größe!\n\
Aber Du mußt Dich\n\
vorsehen!\n\
Bowser wird nicht so\n\
leicht zu besiegen sein.\n\
Ich gebe Dir diesen\n\
Stern, um Dir meine\n\
Demut zu beweisen.\n\
Trage ihn mit Stolz.\n\
Wenn Du mich wiedersehen\n\
willst, wähle den ersten\n\
Stern des Kursmenüs an!\n\
Bis dann...")

DEFINE_DIALOG(DIALOG_117, SOUND_OBJ2_BOSS_DIALOG_GRUNT, 4, 95, 200, "\
Schritte? Grabräuber?\n\
Wer wagt es, den ewigen\n\
Frieden unseres Grabes\n\
zu stören?\n\
Jenem sei gewiß, daß\n\
wir diesen Frevel mit\n\
aller Härte bestrafen\n\
werden!\n\
Unser Zorn wird ihn\n\
zermalmen und Ra stehe\n\
ihm bei, daß es schnell\n\
gehen wird...")

DEFINE_DIALOG(DIALOG_118, SOUND_OBJ2_BOSS_DIALOG_GRUNT, 6, 95, 200, "\
Wir sind besiegt...\n\
Du hast den Fluch des\n\
Pharaos gebrochen.\n\
Jetzt finden wir unseren\n\
verdienten Frieden. Nimm\n\
als Dank diesen Stern!")

DEFINE_DIALOG(DIALOG_119, NO_SOUND, 6, 30, 200, "\
Grrrr, offensichtlich war\n\
ich ein wenig unachtsam.\n\
Aber ich habe noch immer\n\
die Prinzessin und die\n\
Power-Sterne in meiner\n\
Gewalt!\n\
Ich werde dafür sorgen,\n\
daß Du keine weiteren\n\
Sterne erlangen wirst.\n\
Unser nächstes Treffen\n\
wird einen anderen Sieger\n\
haben...bestimmt!")

DEFINE_DIALOG(DIALOG_120, NO_SOUND, 4, 30, 200, "\
Woaaaah, Du hast schon\n\
wieder gewonnen.\n\
Hat mich etwa die Kraft\n\
der Sterne verlassen?\n\
Betrachte diesen Kampf\n\
als Unentschieden!\n\
Das nächste Mal bin ich\n\
besser vorbereitet!\n\
Ich warte am höchsten\n\
Punkt des Schlosses auf\n\
Dich...dort werden wir\n\
sehen, wer stärker ist!")

DEFINE_DIALOG(DIALOG_121, NO_SOUND, 6, 30, 200, "\
Aaaaaarrrgghh!\n\
Du...hast...gewonnen!\n\
Ich wollte mit meinen\n\
Truppen dieses Schloss\n\
erobern, doch Du hast\n\
meine Pläne vereitelt.\n\
Der Frieden kehrt\n\
zurück in die Welten\n\
der Wandgemälde...leider!\n\
Jetzt bleibt mir nichts\n\
anderes mehr übrig, als\n\
mein Ende abzuwarten...")

DEFINE_DIALOG(DIALOG_122, NO_SOUND, 4, 30, 200, "\
Das Schwarze Loch\n\
Rechts: Arbeitsplattform\n\
\t\tNebellabyrinth\n\
Links:  Höhlensee")

DEFINE_DIALOG(DIALOG_123, NO_SOUND, 3, 30, 200, "\
Titanenhöhle\n\
Rechts: Wasserfall\n\
Links:  Grüner Schalter")

DEFINE_DIALOG(DIALOG_124, NO_SOUND, 5, 30, 200, "\
Arbeitsplattform\n\
Zur Beachtung:\n\
Aktiviere einen der\n\
Richtungspfeile, um die\n\
Plattform zu bewegen.")

DEFINE_DIALOG(DIALOG_125, NO_SOUND, 3, 30, 200, "\
Rechts ist der Ausgang\n\
des Nebellabyrinths. Bitte\n\
Eingang links benutzen!")

DEFINE_DIALOG(DIALOG_126, NO_SOUND, 3, 30, 200, "\
Oben:   Schwarzes Loch\n\
Rechts: Arbeitsplattform\n\
\t\tNebellabyrinth")

DEFINE_DIALOG(DIALOG_127, NO_SOUND, 4, 30, 200, "\
Höhlensee\n\
Rechts: Titanenhöhle\n\
Links:  Verlassene Mine\n\
\t\t(Geschlossen!!!)\n\
Hier lebt ein Seemonster.\n\
Führe auf seinem Rücken\n\
eine Stampfattacke aus,\n\
damit es den Kopf senkt!")

DEFINE_DIALOG(DIALOG_128, SOUND_OBJ_KING_BOBOMB_TALK, 4, 95, 200, "\
Hey, es ist gegen die\n\
königlichen Regeln, den\n\
Herrscher aus dem Ring\n\
zu werfen!")

DEFINE_DIALOG(DIALOG_129, NO_SOUND, 4, 30, 200, "\
Herzlich willkommen!\n\
In diesem Kurs kannst\n\
Du den blauen Schalter\n\
entdecken.\n\
Löst Du ihn aus, kannst\n\
Du in allen blauen Blöcken\n\
Tarnkappen finden, die\n\
Dich unsichtbar machen!\n\
Auf diese Weise kannst Du\n\
durch bestimmte Wände\n\
gehen und Gegnern\n\
ungesehen ausweichen.")

DEFINE_DIALOG(DIALOG_130, NO_SOUND, 4, 30, 200, "\
Herzlich willkommen!\n\
In diesem Kurs kannst\n\
Du den grünen Schalter\n\
entdecken.\n\
Löst Du ihn aus, kannst\n\
Du in allen grünen Blöcken\n\
Titanenkappen finden, die\n\
Dich unbesiegbar machen.\n\
Bist Du mit diesen Mützen\n\
ausgerüstet, kannst Du\n\
unter Wasser laufen und\n\
brauchst nicht zu atmen.")

DEFINE_DIALOG(DIALOG_131, NO_SOUND, 4, 30, 200, "\
Herzlich willkommen!\n\
In diesem Kurs kannst\n\
Du den roten Schalter\n\
entdecken.\n\
Löst Du ihn aus, kannst\n\
Du in allen roten Blöcken\n\
Federkappen finden, mit\n\
denen Du fliegen kannst.\n\
Benutze den Dreisprung,\n\
um den Flug zu starten.\n\
Die Kontrolle funktioniert\n\
wie bei einem Flugzeug.\n\
Bewege den Analog-Stick\n\
nach vorne, um zu sinken,\n\
und nach hinten, um zu\n\
steigen!")

DEFINE_DIALOG(DIALOG_132, SOUND_OBJ_BIG_PENGUIN_YELL, 3, 30, 200, "\
Tsetsetse...Mario!\n\
Du versuchst doch nicht\n\
etwa, mich zu betrügen?\n\
Abkürzungen sind nicht\n\
erlaubt.\n\
Du bist disqualifiziert!")

DEFINE_DIALOG(DIALOG_133, NO_SOUND, 5, 30, 200, "\
Ich freue mich, Dich zu\n\
sehen. Die Prinzessin...\n\
ich...und alle anderen\n\
sind in den Wänden des\n\
Schlosses gefangen.\n\
Bowser hat die Sterne\n\
entwendet und benutzt\n\
sie, um seine eigenen\n\
Welten in den Gemälden\n\
zu erschaffen.\n\
Du mußt die Power-Sterne\n\
finden. Mit ihrer Hilfe\n\
kannst Du Bowsers Siegel\n\
an den Türen des Schlosses\n\
brechen.\n\
Im Erdgeschoß gibt es\n\
vier Welten. Beginne in\n\
der Welt der Bob-Ombs.\n\
Das ist die einzige Tür,\n\
die nicht versiegelt ist.\n\
Hast Du acht Sterne\n\
gesammelt, kannst Du die\n\
Tür mit dem großen Stern\n\
öffnen. Dahinter befindet\n\
sich die Prinzessin!")

DEFINE_DIALOG(DIALOG_134, NO_SOUND, 4, 30, 200, "\
Am Beginn jeder Welt\n\
erhältst Du einen Hinweis,\n\
wo der nächste Stern\n\
zu finden ist.\n\
Du kannst sie in beliebiger\n\
Reihenfolge sammeln, aber\n\
einige erscheinen nur unter\n\
bestimmten Bedingungen.\n\
Nachdem Du ein paar\n\
Sterne gefunden hast,\n\
kannst Du Dich in anderen\n\
Welten umsehen.")

DEFINE_DIALOG(DIALOG_135, NO_SOUND, 5, 30, 200, "\
Bowser hat sich die\n\
Power-Sterne unter den\n\
Nagel gerissen. In jeder\n\
Welt hat er sechs Sterne\n\
versteckt.\n\
Manche der Sterne kannst\n\
Du aber erst finden, wenn\n\
Du die farbigen Schalter\n\
in den Schalterpalästen\n\
ausgelöst hast.\n\
Bereits gefundene Sterne\n\
kannst Du zu Beginn jeder\n\
Welt sehen. Triff besiegte\n\
Gegner, indem Du erneut\n\
ihren Stern anwählst.")

DEFINE_DIALOG(DIALOG_136, NO_SOUND, 4, 30, 200, "\
Wow, Du hast bereits so\n\
viele Sterne gefunden?\n\
Ich bin sicher, das war\n\
kein Kinderspiel!\n\
Ich habe noch einige Tips,\n\
die Dir bei der Suche nach\n\
den Power-Sternen von\n\
Nutzen sein könnten:\n\
Sammle Münzen, um Deine\n\
Energie aufzufüllen. Ihre\n\
Farbe entscheidet, wieviel\n\
Energie Du erhältst.\n\
Gelbe Münzen geben eine,\n\
rote Münzen zwei und\n\
blaue Münzen sogar fünf\n\
Energieeinheiten zurück.\n\
Damit die blauen Münzen\n\
sichtbar werden, mußt\n\
Du ihre Schalter in den\n\
Boden stampfen.\n\
Halte Verletzungen gering,\n\
indem Du bei Stürzen im\n\
letzen Moment eine\n\
Stampfattacke ausführst.")

DEFINE_DIALOG(DIALOG_137, NO_SOUND, 5, 30, 200, "\
Vielen Dank! Du hast\n\
schon sehr viele Sterne\n\
gesammelt und Bowser in\n\
ein höheres Stockwerk\n\
verbannt.\n\
Wußtest Du eigentlich,\n\
daß Du in jeder Welt\n\
einen geheimen Stern\n\
erhältst, wenn Du\n\
100 Münzen sammelst?")

DEFINE_DIALOG(DIALOG_138, NO_SOUND, 4, 30, 200, "\
Unten:  Höhlensee\n\
Links:  Schwarzes Loch\n\
Rechts: Nebellabyrinth\n\
\t\t(Geschlossen!!!)")

DEFINE_DIALOG(DIALOG_139, NO_SOUND, 4, 30, 200, "\
Automatische Plattform\n\
Sobald Du diese Plattform\n\
betrittst, setzt sie sich\n\
in Bewegung.\n\
Sie folgt einem\n\
programmierten Kurs und\n\
verschwindet automatisch,\n\
wenn Du sie verläßt.")

DEFINE_DIALOG(DIALOG_140, NO_SOUND, 6, 30, 200, "\
Arbeitsplattform\n\
Rechts: Nebellabyrinth\n\
\t\tEingang\n\
Links:  Schwarzes Loch\n\
\t\tAufzug 1\n\
Pfeil:  Standort")

DEFINE_DIALOG(DIALOG_141, NO_SOUND, 5, 150, 200, "\
Du hast einen der\n\
gestohlenen Power-Sterne\n\
gefunden.\n\
Damit kannst Du einige\n\
versiegelte Türen öffnen.\n\
Versuch's mal im Zimmer\n\
der Prinzessin im\n\
1. Stock oder in\n\
Wummps Festung im\n\
Erdgeschoß der Vorhalle.\n\
Halte Bowser auf und\n\
rette uns und die\n\
Power-Sterne.\n\
Wir zählen auf Dich und\n\
drücken Dir die Daumen.")

DEFINE_DIALOG(DIALOG_142, NO_SOUND, 5, 150, 200, "\
Du hast drei magische\n\
Power-Sterne gesammelt.\n\
Jetzt kannst Du jede Tür\n\
öffnen, deren Siegel eine\n\
Drei trägt.\n\
Du kannst die Türen so oft\n\
passieren, wie Du willst.\n\
Aber paß auf: In höheren\n\
Stockwerken werden die\n\
Gegner viel stärker!")

DEFINE_DIALOG(DIALOG_143, NO_SOUND, 6, 150, 200, "\
Du hast acht Sterne\n\
gesammelt. Jetzt kannst\n\
Du die Tür mit dem\n\
großen Stern öffnen.\n\
Die Prinzessin hält sich\n\
dahinter auf!")

DEFINE_DIALOG(DIALOG_144, NO_SOUND, 5, 150, 200, "\
Du hast 30 Power-Sterne\n\
gesammelt. Jetzt kannst\n\
Du die Tür mit dem\n\
großen Stern öffnen. Aber\n\
warte noch einen Moment!\n\
Hast Du die beiden Säulen\n\
in den Boden gestampft?\n\
Und Du hast doch wohl\n\
nicht Deine Mütze\n\
verloren, oder?\n\
Wenn ja, mußt Du den\n\
Riesengeier mit einer\n\
Stampfattacke besiegen!\n\
Ach ja: Bowser hält sich\n\
jetzt im Untergrund auf.")

DEFINE_DIALOG(DIALOG_145, NO_SOUND, 4, 150, 200, "\
Du hast 50 Power-Sterne\n\
gesammelt. Jetzt kannst\n\
Du die Tür mit dem großen\n\
Stern im 3. Stock öffnen.\n\
Hast Du bereits alle\n\
Schalterpaläste gefunden?\n\
Diese besonderen Mützen\n\
sind sehr nützlich.")

DEFINE_DIALOG(DIALOG_146, NO_SOUND, 6, 150, 200, "\
Du hast 70 Power-Sterne\n\
gesammelt. Jetzt kannst\n\
Du das Geheimnis der\n\
endlosen Treppe lüften.\n\
Begib Dich zum finalen\n\
Duell mit Bowser!")

DEFINE_DIALOG(DIALOG_147, NO_SOUND, 4, 30, 200, "\
Bevor Du weitergehst,\n\
solltest Du nach den\n\
versteckten Schaltern\n\
suchen.\n\
Hast Du sie gefunden,\n\
kannst Du in allen\n\
bunten Blöcken besondere\n\
Mützen finden.\n\
In roten Blöcken findest\n\
Du Federkappen, in\n\
grünen Titanenkappen\n\
und in blauen Tarnkappen.\n\
Allerdings mußt Du bereits\n\
einige Sterne besitzen, um\n\
die Schalterpaläste finden\n\
zu können!")

DEFINE_DIALOG(DIALOG_148, NO_SOUND, 4, 30, 200, "\
Achtung, Achtung!\n\
Versuche bloß nicht, den\n\
Berg mit dem Dreisprung\n\
zu erreichen!\n\
Außerdem ist das Wasser\n\
sehr kalt - Deine Fitness\n\
könnte bei einem Bad\n\
großen Schaden nehmen!")

DEFINE_DIALOG(DIALOG_149, NO_SOUND, 5, 30, 200, "\
Willkommen auf der\n\
Rutschbahn der Prinzessin.\n\
Hier befindet sich ein\n\
Stern, den Bowser nicht\n\
entdeckt hat.\n\
Drücke den Analog-Stick\n\
nach vorne, um zu\n\
beschleunigen.\n\
Wenn Du sehr schnell bist,\n\
erhältst Du den Stern.")

DEFINE_DIALOG(DIALOG_150, SOUND_OBJ_WIGGLER_TALK, 5, 30, 200, "\
Aaaaaah! Sieh Dir diese\n\
Schweinerei an. Du hast\n\
meine Wohnung überflutet.\n\
Meine ganze Einrichtung\n\
ist ruiniert!\n\
Ich bin stinksauer! Alles\n\
läuft schief, seit ich\n\
diesen dämlichen Stern\n\
gefunden habe. Ich...\n\
Ich...werde...")

DEFINE_DIALOG(DIALOG_151, SOUND_OBJ_WIGGLER_TALK, 5, 30, 200, "\
Ich halte das nicht mehr\n\
aus - zuerst überflutest\n\
Du meine Wohnung und\n\
dann hüpfst Du auch noch\n\
auf mir herum!?\n\
Mir reicht's! Ich werde\n\
Dir richtiges Benehmen\n\
beibringen! Du...Ich...\n\
werde Dich...Ich werde\n\
Dich lehren...")

DEFINE_DIALOG(DIALOG_152, SOUND_OBJ_WIGGLER_TALK, 4, 30, 200, "\
Ooooh, halt, halt! Ich\n\
gebe auf. Hier, nimm\n\
den Stern. Ich brauche\n\
ihn nicht mehr.\n\
Ich kann mir die Sterne\n\
durch das Loch in der\n\
Decke ansehen, wann\n\
immer ich möchte.")

DEFINE_DIALOG(DIALOG_153, NO_SOUND, 4, 30, 200, "\
Hey, wer ist da? Wer\n\
meint, auf mir 'rumlaufen\n\
zu müssen? Ein Eisfloh\n\
oder eine Schneefliege?\n\
Egal, was es ist, auf\n\
jeden Fall stört es mich.\n\
Aber mein Eisatem wird\n\
mich davon befreien!")

DEFINE_DIALOG(DIALOG_154, NO_SOUND, 6, 30, 200, "\
Paß gut auf Deine Mütze\n\
auf - ohne sie bist Du\n\
leicht verletzbar!\n\
Wenn Du sie verloren hast,\n\
findest Du sie in\n\
derselben Welt wieder.")

DEFINE_DIALOG(DIALOG_155, NO_SOUND, 3, 30, 200, "\
Hi, Mario! Ich erzähle\n\
Dir einige Geheimnisse\n\
des Schlosses:\n\
Es heißt, daß der Spiegel\n\
des Spiegelsaals magische\n\
Fähigkeiten besitzt.\n\
Er soll Dinge zeigen, die\n\
Du mit bloßem Auge nicht\n\
erkennen kannst.\n\
Aber das ist bestimmt\n\
Unfug und lediglich\n\
dummer Aberglaube.\n\
Bei der versunkenen\n\
Stadt ist das schon\n\
etwas anderes.\n\
Du kannst den Pegel des\n\
Wassers durch die Sprünge\n\
ins Gemälde beeinflussen.\n\
Am besten schaust Du Dir\n\
die Wirkung direkt im\n\
Gemälde an!")

DEFINE_DIALOG(DIALOG_156, NO_SOUND, 4, 30, 200, "\
Diese Uhr ist wirklich\n\
ungewöhnlich. Ihr Inneres\n\
verändert sich im Laufe\n\
der Zeit.")

DEFINE_DIALOG(DIALOG_157, NO_SOUND, 5, 30, 200, "\
Achte auf den Treibsand!\n\
Solltest Du einmal darin\n\
versinken, findet Dein\n\
Gemäldebesuch ein rasches\n\
Ende.\n\
Schwarze Löcher sind\n\
bodenlose Fallen. Du wirst\n\
das Gemälde neu betreten\n\
müssen, wenn Du in eines\n\
dieser Löcher hineinfällst.")

DEFINE_DIALOG(DIALOG_158, NO_SOUND, 6, 30, 200, "\
Mit dem richtigen Timing\n\
kannst Du sehr hoch\n\
springen. Den Dreisprung\n\
kannst Du ausführen, wenn\n\
Du rennst und dreimal\n\
hintereinander springst.\n\
Wandsprünge sind eine\n\
effektive Methode, um\n\
hohe Stellen zu erreichen.\n\
Springe an eine Wand.\n\
Sobald Du sie berührst,\n\
mußt Du erneut springen!")

DEFINE_DIALOG(DIALOG_159, NO_SOUND, 4, 30, 200, "\
Gehst Du in die Hocke und\n\
drückst den Sprungknopf,\n\
kannst Du einen\n\
Rückwärtssalto ausführen.\n\
Du machst einen\n\
Weitsprung, wenn Du im\n\
Rennen den Ⓩ-Knopf\n\
drückst und springst.")

DEFINE_DIALOG(DIALOG_160, NO_SOUND, 6, 30, 200, "\
Drücke im Rennen den\n\
Ⓑ-Knopf, um einen\n\
Hechtsprung zu machen.\n\
Mit dem Ⓐ- oder Ⓑ-Knopf\n\
kommt Mario auf die Füße\n\
zurück.")

DEFINE_DIALOG(DIALOG_161, SOUND_GENERAL_YOSHI_TALK, 5, 30, 200, "\
Hey... Mario!!! Seit\n\
unserem letzten Abenteuer\n\
ist viel Zeit vergangen.\n\
Ich freue mich, Dich\n\
wiederzusehen.\n\
Man erzählte mir, daß ich\n\
Dich hier treffen könnte,\n\
aber ich hatte die\n\
Hoffnung schon fast\n\
aufgegeben.\n\
Ist es wahr? Du hast\n\
Bowser besiegt und alle\n\
gestohlenen Power-Sterne\n\
zurückerobert?\n\
Unglaublich!!!\n\
Und die Prinzessin?\n\
Du hast sie gerettet?\n\
Cool! Aber ich wußte\n\
natürlich, daß Du es\n\
schaffen würdest.\n\
Nun habe ich noch eine\n\
ganz spezielle Nachricht\n\
für Dich:\n\
Danke, daß Du SUPER\n\
MARIO 64 gespielt hast.\n\
Du hast jetzt alle Sterne\n\
gefunden und damit das\n\
Spiel beendet. Aber wir\n\
haben noch eine kleine\n\
Überraschung für Dich!\n\
Wir hoffen, sie wird Dir\n\
gefallen, denn du kannst\n\
das Spiel nochmals unter\n\
veränderten Bedingungen\n\
spielen. Viel Spaß!\n\
Es könnte sein, daß Du\n\
dabei neue Entdeckungen\n\
machst, die Dir bis jetzt\n\
verborgen geblieben sind.\n\
Das Super Mario 64 Team")

DEFINE_DIALOG(DIALOG_162, NO_SOUND, 4, 30, 200, "\
Nein, nein, nein! Nicht\n\
Du schon wieder! Ich habe\n\
jetzt wirklich keine Zeit,\n\
über Sterne zu quatschen!\n\
Hier, nimm ihn und laß\n\
mich runter. Ich habe noch\n\
wichtige Geschäfte zu\n\
erledigen!")

DEFINE_DIALOG(DIALOG_163, NO_SOUND, 5, 30, 200, "\
Ich glaub's nicht! Du hast\n\
mich besiegt...wie konnte\n\
das nur geschehen. Meine\n\
Truppen, die Sterne...\n\
alles umsonst!\n\
Was??? Es gab insgesamt\n\
120 Sterne im Schloss zu\n\
finden? Dann habe ich\n\
wohl einige bei meiner\n\
Suche übersehen!\n\
Jetzt kehrt der Frieden\n\
zurück in die Welten der\n\
Gemälde - was für ein\n\
fürchterlicher Gedanke...\n\
Aaaaaarrrgghhhh!!!\n\
Das kann ich mir nicht\n\
länger ansehen - ich\n\
verschwinde. So long...\n\
Bis zum nächsten Mal.\n\
Ich komme wieder...")

DEFINE_DIALOG(DIALOG_164, NO_SOUND, 6, 30, 200, "\
Hi! Wie geht's, Mario?\n\
Ich bin zwar etwas außer\n\
Übung, aber ein Rennen\n\
gegen den Schlidderkönig\n\
ist immer eine große\n\
Herausforderung.\n\
Also, Kumpel, wie steht's?\n\
Sollen wir ein kleines\n\
Match wagen?\n\
\n\
\tOkay!\t  Später!")

DEFINE_DIALOG(DIALOG_165, NO_SOUND, 4, 30, 200, "\
Sei vorsichtig, wenn Du\n\
um den Pfahl herumrennst,\n\
damit Dir nicht schlecht\n\
wird!")

DEFINE_DIALOG(DIALOG_166, NO_SOUND, 5, 30, 200, "\
Ich mache gerade einen\n\
kleinen Waldlauf. Komm\n\
doch später wieder\n\
vorbei!\n\
Dein Freund Koopa")

DEFINE_DIALOG(DIALOG_167, NO_SOUND, 4, 30, 200, "\
Dort vorne ist das\n\
Schloss der Prinzessin.\n\
Sieh Dich zunächst hier\n\
im Schlossgarten um.\n\
Die Steuerung ist ganz\n\
einfach:\n\
Drücke den Ⓐ-Knopf, um\n\
zu springen.\n\
Schläge kannst Du mit\n\
dem Ⓑ-Knopf ausführen.\n\
Drückst Du den Ⓩ-Knopf,\n\
duckt sich Mario.\n\
Schilder kannst Du mit\n\
dem Ⓐ- oder Ⓑ-Knopf\n\
lesen, wenn Du direkt\n\
davor stehst.")

DEFINE_DIALOG(DIALOG_168, SOUND_OBJ_WIGGLER_TALK, 4, 30, 200, "\
Bist Du immer noch hier?\n\
Findest Du nicht, daß\n\
Du bereits genug Schaden\n\
angerichtet hast?")

DEFINE_DIALOG(DIALOG_169, NO_SOUND, 4, 30, 200, "\
Dies ist die erste und\n\
letzte Warnung:\n\
Das Betreten dieser\n\
Höhle ist verboten!!!\n\
Wer sich hier ohne meine\n\
Erlaubnis Zutritt\n\
verschafft, handelt sich\n\
eine Menge Ärger ein!!!")
