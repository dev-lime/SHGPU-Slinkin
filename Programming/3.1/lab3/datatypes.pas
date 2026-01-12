unit DataTypes;

interface

type
  TPerson = record
    FullName: AnsiString;
    Gender: AnsiString;
    BirthDate: AnsiString;
    IDNumber: AnsiString;
    ChildrenIDs: array of AnsiString;
  end;

  TPeopleArray = array of TPerson;

implementation

end.

