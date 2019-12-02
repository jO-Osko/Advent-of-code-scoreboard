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
    SERIALIZABLE_VARS = ["name", "surname", "github_link", "github_repo_link", "aoc_id", "solution_data", ]

    def __init__(self, name: str, surname: str, github_link: Optional[str] = None,
                 github_repo_link: Optional[str] = None, aoc_id: Optional[int] = None,
                 solution_data: SolutionDict = None):
        super().__init__()
        self.name = name
        self.surname = surname
        self.github_link = github_link
        self.github_repo_link = github_repo_link
        self.aoc_id = aoc_id

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


AOC_INPUT_FILE = "static/aoc_raw_prog1_2019.json"
BASE_FILE = "static/data.json"
aoc_data = json.loads(open(AOC_INPUT_FILE).read())
base_data = json.loads(open(BASE_FILE).read())

users = [
    UserInfo.from_json_dict(j) for j in base_data
]

users_dict: Dict[int, UserInfo] = {
    user.aoc_id: user for user in users if user.aoc_id is not None
}

for aoc_id, user in aoc_data["members"].items():
    aoc_id = int(aoc_id)
    if aoc_id not in users_dict:
        print("Unknown user:", user["name"])
        continue
    base_user = users_dict[aoc_id]

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

out_data = json.dumps(users, indent=2, cls=JSONSerializableEncoder)
open(BASE_FILE, "w").write(out_data)
