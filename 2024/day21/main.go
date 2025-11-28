package main

import (
	_ "embed"
	"flag"
	"fmt"
	"strconv"
	"strings"
	"time"

	"golang.design/x/clipboard"
)

//go:embed input.txt
var input string

func init() {
	// do this in init (not main) so test file has same input
	input = strings.TrimRight(input, "\n")
	if len(input) == 0 {
		panic("empty input.txt file")
	}
}

func main() {
	var part int
	flag.IntVar(&part, "part", 1, "part 1 or 2")
	flag.Parse()
	fmt.Println("Running part", part)
	err := clipboard.Init()
	if err != nil {
		panic(err)
	}
	t := time.Now()
	if part == 1 {
		ans := part1(input)
		clipboard.Write(clipboard.FmtText, []byte(fmt.Sprintf("%v", ans)))
		fmt.Println("Output:", ans)
		ct := 0
		ot := ""
		for _, s := range "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A" {
			if s == 'A' {
				ot += strconv.Itoa(ct) + "A"
				ct = 0
			}
			ct++
		}
		fmt.Println(ot)

	} else {
		ans := part2(input)
		clipboard.Write(clipboard.FmtText, []byte(fmt.Sprintf("%v", ans)))
		fmt.Println("Output:", ans)
	}
	fmt.Println("Time", time.Since(t))
}

func part1(input string) int {
	// fStage := make(map[string][]string)
	baseBoard := [][]rune{{'7', '8', '9'}, {'4', '5', '6'}, {'1', '2', '3'}, {' ', '0', 'A'}}
	keypad := [][]rune{{' ', '^', 'A'}, {'<', 'v', '>'}}

	var currPos [2]int
	var shortestPath = func(board [][]rune, pos [2]int, destSymbol rune) (string, [2]int) {
		out := ""
		for i, row := range board {
			for j, sybl := range row {
				if sybl == destSymbol {
					if pos[0] > i {
						out += strings.Repeat("^", pos[0]-i)
					} else if pos[0] < i {
						out += strings.Repeat("v", i-pos[0])
					}
					if pos[1] > j {
						out += strings.Repeat("<", pos[1]-j)
					} else if pos[1] < j {
						out += strings.Repeat(">", j-pos[1])
					}
					return out, [2]int{i, j}
				}
			}
		}
		return out, pos
	}

	for _, line := range strings.Split(input, "\n") {
		baseInp := line
		var inp string
		currPos = [2]int{3, 2}
		for _, sybl := range baseInp {
			pth, ps := shortestPath(baseBoard, [2]int{currPos[0], currPos[1]}, sybl)
			inp += pth + "A"
			currPos = ps
		}
		fmt.Println(inp)
		var out string
		for x := 0; x < 2; x++ {
			out = ""
			currPos = [2]int{0, 2}
			for _, sybl := range inp {
				pth, ps := shortestPath(keypad, [2]int{currPos[0], currPos[1]}, sybl)
				out += pth + "A"
				currPos = ps
			}
			inp = out
			fmt.Println(out, baseInp)
		}
		ct := 0
		ot := ""
		for _, s := range out {
			if s == 'A' {
				ot += strconv.Itoa(ct) + "A"
				ct = 0
			}
			ct++
		}
		fmt.Println(ot)
		break
	}
	return 0
}

func part2(input string) int {
	panic("not implemented")
}
