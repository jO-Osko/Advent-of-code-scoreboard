import json
from json import JSONEncoder
from typing import Optional, List, Dict, Any, TypeVar, Type

T = TypeVar("T", bound="JSONSerializable")
SolutionDict = Dict[int, Dict[int, "AssigmentResult"]]


class JSONSerializable:
    SERIALIZABLE_VARS: List[str] = []

    def __init__(self, *args, **kwargs):
        pass

    def to_json_dict(self) -> Dict[str, Any]:
        return {j: getattr(self, j) for j in self.SERIALIZABLE_VARS}

    @classmethod
    def from_json_dict(cls: Type[T], json_dict: Dict[str, Any]) -> "T":
        return cls.from_json_dump(**{j: json_dict[j] for j in cls.SERIALIZABLE_VARS})

    @classmethod
    def from_json_dump(cls: Type[T], *args, **kwargs) -> T:
        return cls(*args, **kwargs)


class JSONSerializableEncoder(JSONEncoder):
    def default(self, o: JSONSerializable):
        return o.to_json_dict()


class AssigmentResult(JSONSerializable):
    SERIALIZABLE_VARS = ["language", "solution_confirmed", "solved", "solved_time"]

    def __init__(self, language: Optional[str] = None, solution_confirmed: bool = False,
                 solved: bool = False, solved_time: Optional[int] = None):
        super().__init__()
        self.language = language
        self.solution_confirmed = solution_confirmed
        self.solved = solved
        self.solved_time = solved_time


class UserInfo(JSONSerializable):
    SERIALIZABLE_VARS = ["name", "surname", "github_link", "github_repo_link",
                         "aoc_id", "aoc_name", "solution_data", ]

    def __init__(self, name: str, surname: str, github_link: Optional[str] = None,
                 github_repo_link: Optional[str] = None, aoc_id: Optional[int] = None,
                 aoc_name: Optional[str] = None, solution_data: SolutionDict = None):
        super().__init__()
        self.name = name
        self.surname = surname
        self.github_link = github_link
        self.github_repo_link = github_repo_link
        self.aoc_id = aoc_id
        self.aoc_name = aoc_name

        if solution_data is not None:
            self.solution_data = solution_data
        else:
            self.solution_data = {}

    @classmethod
    def from_json_dump(cls: Type[T], *args, **kwargs) -> T:

        solution_data = kwargs["solution_data"]
        sol_data: SolutionDict = {}
        for day, info in solution_data.items():
            dic: Dict[int, "AssigmentResult"] = {}
            sol_data[int(day)] = dic
            for star, s_info in info.items():
                dic[int(star)] = AssigmentResult.from_json_dump(**s_info)

        kwargs["solution_data"] = sol_data
        return cls(**kwargs)


AOC_INPUT_FILE = "static/aoc_final.json"
BASE_FILE = "static/data.json"
aoc_data = json.loads(open(AOC_INPUT_FILE).read())
base_data = json.loads(open(BASE_FILE).read())

users = [
    UserInfo.from_json_dict(j) for j in base_data
]

users_dict: Dict[int, UserInfo] = {
    user.aoc_id: user for user in users if user.aoc_id is not None
}

new_users = [

]

for aoc_id, user in aoc_data["members"].items():
    aoc_id = int(aoc_id)
    if aoc_id not in users_dict:
        usr = UserInfo(
            name="",
            surname="", github_link="", github_repo_link="", aoc_id=aoc_id,
            aoc_name=user["name"]
        )
        new_users.append(usr)
        print("Unknown user:", user["name"])
        continue
    base_user = users_dict[aoc_id]
    if not base_user.aoc_name:
        base_user.aoc_name = user["name"]
    for day_n, day_info in user["completion_day_level"].items():
        base_day_info = base_user.solution_data.setdefault(int(day_n), dict())

        day_1 = base_day_info.setdefault(1, AssigmentResult())
        day_1_info = day_info.get("1")
        if day_1_info is not None:
            day_1.solved = True
            day_1.solved_time = int(day_1_info["get_star_ts"])
        day_2 = base_day_info.setdefault(2, AssigmentResult())
        day_2_info = day_info.get("2")
        if day_2_info is not None:
            day_2.solved = True
            day_2.solved_time = int(day_2_info["get_star_ts"])


def sort_dict(d):
    return {
        k: (sort_dict(v) if isinstance(v, dict) else v)
        for k, v in sorted(d.items(), key=lambda x: int(x[0]) if x[0].isdigit() else len(x[0]) )
    }


users.sort(key=lambda x: x.aoc_id)

full_users = users + new_users
out_users = json.dumps(full_users, indent=4, cls=JSONSerializableEncoder,
                       ensure_ascii=False)
open(BASE_FILE, "w").write(out_users)

open(AOC_INPUT_FILE, "w", encoding="utf-8").write(
    json.dumps(sort_dict(aoc_data), indent=2, ensure_ascii=False)
)

# Manjkajoči na scoreboardu:
# https://github.com/golobluka/AdventOfCode Luka Golob
# https://github.com/MTrdin/resevanje_advent_of_code Meta Trdin
# https://github.com/matejmelansek/AdventOfCode Matej Melanšek
# https://github.com/NinaKlem/PROG1-AoC Nina Klemenčič
