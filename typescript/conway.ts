const canvas = document.querySelector<HTMLCanvasElement>("#game");
const ctx = canvas.getContext("2d");
const TILE_SIZE = 20;
const TILES_X = canvas.width / TILE_SIZE;
const TILES_Y = canvas.height / TILE_SIZE;
ctx.fillStyle = "rgb(100, 240, 150)";
ctx.strokeStyle = "rgb(90, 90, 90)";
ctx.lineWidth = 1;

type Cell = [number, number];
type Cells = Set<Cell>;

function drawBorders() {
    for (let i = 0; i < TILES_X; i++) {
        ctx.beginPath();
        ctx.moveTo(i * TILE_SIZE - 0.5, 0);
        ctx.lineTo(i * TILE_SIZE - 0.5, canvas.height);
        ctx.stroke();
    }
    for (let j = 0; j < TILES_Y; j++) {
        ctx.beginPath();
        ctx.moveTo(0, j * TILE_SIZE - 0.5);
        ctx.lineTo(canvas.width, j * TILE_SIZE - 0.5);
        ctx.stroke();
    }
}

function drawCells( cells: Cells) {
    for (let cell of cells) {
        ctx.fillRect(TILE_SIZE * cell[0], TILE_SIZE * cell[1], TILE_SIZE, TILE_SIZE);
    }
}

function neighbors(cell: Cell): Cells {
    let ret = new Set<Cell>([
        [cell[0] - 1, cell[1] - 1],
        [cell[0] - 1, cell[1]    ],
        [cell[0] - 1, cell[1] + 1],
        [cell[0]    , cell[1] - 1],
        [cell[0]    , cell[1] + 1],
        [cell[0] + 1, cell[1] - 1],
        [cell[0] + 1, cell[1]    ],
        [cell[0] + 1, cell[1] + 1],
    ]);
    console.log("neighbors: ", ret);
    return ret;
}

function cellsWithNeighbors(cells: Cells): Cells {
    let ret = new Set<Cell>();
    for (let cell of cells) {
        ret.add(cell);
        for (let neighbor of neighbors(cell)) {
            ret.add(neighbor);
        }
    }
    console.log("cellsWithNeighbors: ", ret);
    return ret;
}

function survive2Spawn3(cells: Cells, cell: Cell): boolean {
    const intersection = new Set(
        Array.from(neighbors(cell)).filter(x => cells.has(x))
    );
    console.log("Intersection:", intersection);
    const count = Array.from(intersection).length;
    console.log("Count: ", count);
    return ((count == 2 && cells.has(cell)) || count == 3);
}

type Condition = (cells: Cells, cell: Cell) => void;
function next(cond: Condition, cells: Cells): Cells {
    return new Set<Cell>(
        Array.from(cellsWithNeighbors(cells)).filter(x => cond(cells, x))
    );
}

function sleep(ms: number) {
    return new Promise(resolve => setTimeout(resolve, ms));
}

async function runConway() {
    let cells = new Set<Cell>([[2,1], [2,2], [2,3]]);
    while(true) {
        ctx.clearRect(0, 0, canvas.width, canvas.height);
        drawBorders();
        drawCells(cells);
        cells = next(survive2Spawn3, cells);
        await sleep(1000);
    }
}

runConway();