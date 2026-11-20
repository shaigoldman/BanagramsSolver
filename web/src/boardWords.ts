/** Read complete contiguous words from the finished grid, including cross-words. */
export function boardWords(rows: string[]): {across: string[]; down: string[]} {
  const wordsIn = (line: string) => line.match(/[a-z]{2,}/gi) ?? [];
  const across = rows.flatMap(wordsIn);
  const width = Math.max(0, ...rows.map(row => row.length));
  const down = Array.from({length: width}, (_, x) =>
    rows.map(row => row[x] ?? ' ').join('')
  ).flatMap(wordsIn);
  // The solver can return a one-tile board when that tile is in its dictionary.
  if (across.length === 0 && down.length === 0) {
    const letters = rows.join('').replace(/[^a-z]/gi, '');
    if (letters.length === 1) across.push(letters);
  }
  return { across, down };
}
