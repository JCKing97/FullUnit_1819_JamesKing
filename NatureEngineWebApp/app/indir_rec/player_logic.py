
class Player:

    def __init__(self, player_id: int):
        self._player_id = player_id
        self._fitness = 0

    def get_id(self) -> int:
        return self._player_id

    def get_fitness(self) -> int:
        return self._fitness

    def update_fitness(self, change: int):
        self._fitness += change
        if self._fitness < 0:
            self._fitness = 0
