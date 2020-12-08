USE `hbef`;
DROP procedure IF EXISTS `remove_quotes_from_notes`;
CREATE PROCEDURE `remove_quotes_from_notes` ()
BEGIN
    update current set notes=replace(notes, '"', '');
    update historical set notes=replace(notes, '"', '');
END
