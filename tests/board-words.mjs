import assert from 'node:assert/strict';
import { boardWords } from '../web/src/boardWords.ts';
assert.deepEqual(boardWords(['   s   ', '   a   ', 'contest']), {across:['contest'],down:['sat']});
assert.deepEqual(boardWords(['cat cat', 'a   a  ', 't   t  ']), {across:['cat','cat'], down:['cat','cat']});
assert.deepEqual(boardWords(['one']), {across:['one'],down:[]});
assert.deepEqual(boardWords(['a']), {across:['a'],down:[]});
assert.deepEqual(boardWords([]), {across:[],down:[]});
console.log('Board word extraction passed');
