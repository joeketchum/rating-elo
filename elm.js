(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.1/optimize for better performance and smaller assets.');


// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (typeof x.$ === 'undefined')
	//*/
	/**/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**_UNUSED/
	var node = args['node'];
	//*/
	/**/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS
//
// For some reason, tabs can appear in href protocols and it still works.
// So '\tjava\tSCRIPT:alert("!!!")' and 'javascript:alert("!!!")' are the same
// in practice. That is why _VirtualDom_RE_js and _VirtualDom_RE_js_html look
// so freaky.
//
// Pulling the regular expressions out to the top level gives a slight speed
// boost in small benchmarks (4-10%) but hoisting values to reduce allocation
// can be unpredictable in large programs where JIT may have a harder time with
// functions are not fully self-contained. The benefit is more that the js and
// js_html ones are so weird that I prefer to see them near each other.


var _VirtualDom_RE_script = /^script$/i;
var _VirtualDom_RE_on_formAction = /^(on|formAction$)/i;
var _VirtualDom_RE_js = /^\s*j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:/i;
var _VirtualDom_RE_js_html = /^\s*(j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:|d\s*a\s*t\s*a\s*:\s*t\s*e\s*x\s*t\s*\/\s*h\s*t\s*m\s*l\s*(,|;))/i;


function _VirtualDom_noScript(tag)
{
	return _VirtualDom_RE_script.test(tag) ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return _VirtualDom_RE_on_formAction.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return _VirtualDom_RE_js.test(value)
		? /**_UNUSED/''//*//**/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return _VirtualDom_RE_js_html.test(value)
		? /**_UNUSED/''//*//**/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlJson(value)
{
	return (typeof _Json_unwrap(value) === 'string' && _VirtualDom_RE_js_html.test(_Json_unwrap(value)))
		? _Json_wrap(
			/**_UNUSED/''//*//**/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		) : value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		message: func(record.message),
		stopPropagation: record.stopPropagation,
		preventDefault: record.preventDefault
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.message;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.stopPropagation;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.preventDefault) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var view = impl.view;
			/**_UNUSED/
			var domNode = args['node'];
			//*/
			/**/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.setup && impl.setup(sendToApp)
			var view = impl.view;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.body);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.title) && (_VirtualDom_doc.title = title = doc.title);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.onUrlChange;
	var onUrlRequest = impl.onUrlRequest;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		setup: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.protocol === next.protocol
							&& curr.host === next.host
							&& curr.port_.a === next.port_.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		init: function(flags)
		{
			return A3(impl.init, flags, _Browser_getUrl(), key);
		},
		view: impl.view,
		update: impl.update,
		subscriptions: impl.subscriptions
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { hidden: 'hidden', change: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { hidden: 'mozHidden', change: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { hidden: 'msHidden', change: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { hidden: 'webkitHidden', change: 'webkitvisibilitychange' }
		: { hidden: 'hidden', change: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		scene: _Browser_getScene(),
		viewport: {
			x: _Browser_window.pageXOffset,
			y: _Browser_window.pageYOffset,
			width: _Browser_doc.documentElement.clientWidth,
			height: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		width: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		height: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			scene: {
				width: node.scrollWidth,
				height: node.scrollHeight
			},
			viewport: {
				x: node.scrollLeft,
				y: node.scrollTop,
				width: node.clientWidth,
				height: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			scene: _Browser_getScene(),
			viewport: {
				x: x,
				y: y,
				width: _Browser_doc.documentElement.clientWidth,
				height: _Browser_doc.documentElement.clientHeight
			},
			element: {
				x: x + rect.left,
				y: y + rect.top,
				width: rect.width,
				height: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



// SEND REQUEST

var _Http_toTask = F3(function(router, toTask, request)
{
	return _Scheduler_binding(function(callback)
	{
		function done(response) {
			callback(toTask(request.expect.a(response)));
		}

		var xhr = new XMLHttpRequest();
		xhr.addEventListener('error', function() { done($elm$http$Http$NetworkError_); });
		xhr.addEventListener('timeout', function() { done($elm$http$Http$Timeout_); });
		xhr.addEventListener('load', function() { done(_Http_toResponse(request.expect.b, xhr)); });
		$elm$core$Maybe$isJust(request.tracker) && _Http_track(router, xhr, request.tracker.a);

		try {
			xhr.open(request.method, request.url, true);
		} catch (e) {
			return done($elm$http$Http$BadUrl_(request.url));
		}

		_Http_configureRequest(xhr, request);

		request.body.a && xhr.setRequestHeader('Content-Type', request.body.a);
		xhr.send(request.body.b);

		return function() { xhr.c = true; xhr.abort(); };
	});
});


// CONFIGURE

function _Http_configureRequest(xhr, request)
{
	for (var headers = request.headers; headers.b; headers = headers.b) // WHILE_CONS
	{
		xhr.setRequestHeader(headers.a.a, headers.a.b);
	}
	xhr.timeout = request.timeout.a || 0;
	xhr.responseType = request.expect.d;
	xhr.withCredentials = request.allowCookiesFromOtherDomains;
}


// RESPONSES

function _Http_toResponse(toBody, xhr)
{
	return A2(
		200 <= xhr.status && xhr.status < 300 ? $elm$http$Http$GoodStatus_ : $elm$http$Http$BadStatus_,
		_Http_toMetadata(xhr),
		toBody(xhr.response)
	);
}


// METADATA

function _Http_toMetadata(xhr)
{
	return {
		url: xhr.responseURL,
		statusCode: xhr.status,
		statusText: xhr.statusText,
		headers: _Http_parseHeaders(xhr.getAllResponseHeaders())
	};
}


// HEADERS

function _Http_parseHeaders(rawHeaders)
{
	if (!rawHeaders)
	{
		return $elm$core$Dict$empty;
	}

	var headers = $elm$core$Dict$empty;
	var headerPairs = rawHeaders.split('\r\n');
	for (var i = headerPairs.length; i--; )
	{
		var headerPair = headerPairs[i];
		var index = headerPair.indexOf(': ');
		if (index > 0)
		{
			var key = headerPair.substring(0, index);
			var value = headerPair.substring(index + 2);

			headers = A3($elm$core$Dict$update, key, function(oldValue) {
				return $elm$core$Maybe$Just($elm$core$Maybe$isJust(oldValue)
					? value + ', ' + oldValue.a
					: value
				);
			}, headers);
		}
	}
	return headers;
}


// EXPECT

var _Http_expect = F3(function(type, toBody, toValue)
{
	return {
		$: 0,
		d: type,
		b: toBody,
		a: toValue
	};
});

var _Http_mapExpect = F2(function(func, expect)
{
	return {
		$: 0,
		d: expect.d,
		b: expect.b,
		a: function(x) { return func(expect.a(x)); }
	};
});

function _Http_toDataView(arrayBuffer)
{
	return new DataView(arrayBuffer);
}


// BODY and PARTS

var _Http_emptyBody = { $: 0 };
var _Http_pair = F2(function(a, b) { return { $: 0, a: a, b: b }; });

function _Http_toFormData(parts)
{
	for (var formData = new FormData(); parts.b; parts = parts.b) // WHILE_CONS
	{
		var part = parts.a;
		formData.append(part.a, part.b);
	}
	return formData;
}

var _Http_bytesToBlob = F2(function(mime, bytes)
{
	return new Blob([bytes], { type: mime });
});


// PROGRESS

function _Http_track(router, xhr, tracker)
{
	// TODO check out lengthComputable on loadstart event

	xhr.upload.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2($elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, $elm$http$Http$Sending({
			sent: event.loaded,
			size: event.total
		}))));
	});
	xhr.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2($elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, $elm$http$Http$Receiving({
			received: event.loaded,
			size: event.lengthComputable ? $elm$core$Maybe$Just(event.total) : $elm$core$Maybe$Nothing
		}))));
	});
}


var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});



function _Time_now(millisToPosix)
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(millisToPosix(Date.now())));
	});
}

var _Time_setInterval = F2(function(interval, task)
{
	return _Scheduler_binding(function(callback)
	{
		var id = setInterval(function() { _Scheduler_rawSpawn(task); }, interval);
		return function() { clearInterval(id); };
	});
});

function _Time_here()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(
			A2($elm$time$Time$customZone, -(new Date().getTimezoneOffset()), _List_Nil)
		));
	});
}


function _Time_getZoneName()
{
	return _Scheduler_binding(function(callback)
	{
		try
		{
			var name = $elm$time$Time$Name(Intl.DateTimeFormat().resolvedOptions().timeZone);
		}
		catch (e)
		{
			var name = $elm$time$Time$Offset(new Date().getTimezoneOffset());
		}
		callback(_Scheduler_succeed(name));
	});
}
var $elm$core$Basics$EQ = {$: 'EQ'};
var $elm$core$Basics$GT = {$: 'GT'};
var $elm$core$Basics$LT = {$: 'LT'};
var $elm$core$List$cons = _List_cons;
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0.a;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var $elm$core$Basics$False = {$: 'False'};
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var $elm$core$Maybe$Nothing = {$: 'Nothing'};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 'Nothing') {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / $elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = {$: 'True'};
var $elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 'Normal':
			return 0;
		case 'MayStopPropagation':
			return 1;
		case 'MayPreventDefault':
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 'External', a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = function (a) {
	return {$: 'NotFound', a: a};
};
var $elm$url$Url$Http = {$: 'Http'};
var $elm$url$Url$Https = {$: 'Https'};
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {fragment: fragment, host: host, path: path, port_: port_, protocol: protocol, query: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 'Nothing') {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Http,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Https,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0.a;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(_Utils_Tuple0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0.a;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return _Utils_Tuple0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(_Utils_Tuple0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0.a;
		return $elm$core$Task$Perform(
			A2($elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			$elm$core$Task$Perform(
				A2($elm$core$Task$map, toMessage, task)));
	});
var $elm$browser$Browser$document = _Browser_document;
var $author$project$Main$All = {$: 'All'};
var $author$project$Main$GotPlayers = function (a) {
	return {$: 'GotPlayers', a: a};
};
var $elm$json$Json$Encode$string = _Json_wrap;
var $author$project$Main$askForAutoSave = _Platform_outgoingPort('askForAutoSave', $elm$json$Json$Encode$string);
var $author$project$Main$askForIgnoredPlayers = _Platform_outgoingPort('askForIgnoredPlayers', $elm$json$Json$Encode$string);
var $author$project$Main$askForTimeFilter = _Platform_outgoingPort('askForTimeFilter', $elm$json$Json$Encode$string);
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Set$Set_elm_builtin = function (a) {
	return {$: 'Set_elm_builtin', a: a};
};
var $elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $elm$core$Set$empty = $elm$core$Set$Set_elm_builtin($elm$core$Dict$empty);
var $elm$http$Http$BadStatus_ = F2(
	function (a, b) {
		return {$: 'BadStatus_', a: a, b: b};
	});
var $elm$http$Http$BadUrl_ = function (a) {
	return {$: 'BadUrl_', a: a};
};
var $elm$http$Http$GoodStatus_ = F2(
	function (a, b) {
		return {$: 'GoodStatus_', a: a, b: b};
	});
var $elm$http$Http$NetworkError_ = {$: 'NetworkError_'};
var $elm$http$Http$Receiving = function (a) {
	return {$: 'Receiving', a: a};
};
var $elm$http$Http$Sending = function (a) {
	return {$: 'Sending', a: a};
};
var $elm$http$Http$Timeout_ = {$: 'Timeout_'};
var $elm$core$Maybe$isJust = function (maybe) {
	if (maybe.$ === 'Just') {
		return true;
	} else {
		return false;
	}
};
var $elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'EQ':
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $elm$core$Dict$Black = {$: 'Black'};
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = {$: 'Red'};
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1.$) {
				case 'LT':
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var $elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.e.d.$ === 'RBNode_elm_builtin') && (dict.e.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var lLeft = _v1.d;
			var lRight = _v1.e;
			var _v2 = dict.e;
			var rClr = _v2.a;
			var rK = _v2.b;
			var rV = _v2.c;
			var rLeft = _v2.d;
			var _v3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _v2.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				rlK,
				rlV,
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					rlL),
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v4 = dict.d;
			var lClr = _v4.a;
			var lK = _v4.b;
			var lV = _v4.c;
			var lLeft = _v4.d;
			var lRight = _v4.e;
			var _v5 = dict.e;
			var rClr = _v5.a;
			var rK = _v5.b;
			var rV = _v5.c;
			var rLeft = _v5.d;
			var rRight = _v5.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.d.d.$ === 'RBNode_elm_builtin') && (dict.d.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var _v2 = _v1.d;
			var _v3 = _v2.a;
			var llK = _v2.b;
			var llV = _v2.c;
			var llLeft = _v2.d;
			var llRight = _v2.e;
			var lRight = _v1.e;
			var _v4 = dict.e;
			var rClr = _v4.a;
			var rK = _v4.b;
			var rV = _v4.c;
			var rLeft = _v4.d;
			var rRight = _v4.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				lK,
				lV,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					lRight,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v5 = dict.d;
			var lClr = _v5.a;
			var lK = _v5.b;
			var lV = _v5.c;
			var lLeft = _v5.d;
			var lRight = _v5.e;
			var _v6 = dict.e;
			var rClr = _v6.a;
			var rK = _v6.b;
			var rV = _v6.c;
			var rLeft = _v6.d;
			var rRight = _v6.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
			var _v1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Black')) {
					if (right.d.$ === 'RBNode_elm_builtin') {
						if (right.d.a.$ === 'Black') {
							var _v3 = right.a;
							var _v4 = right.d;
							var _v5 = _v4.a;
							return $elm$core$Dict$moveRedRight(dict);
						} else {
							break _v2$2;
						}
					} else {
						var _v6 = right.a;
						var _v7 = right.d;
						return $elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _v2$2;
				}
			}
			return dict;
		}
	});
var $elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor.$ === 'Black') {
			if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
				var _v3 = lLeft.a;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					$elm$core$Dict$removeMin(left),
					right);
			} else {
				var _v4 = $elm$core$Dict$moveRedLeft(dict);
				if (_v4.$ === 'RBNode_elm_builtin') {
					var nColor = _v4.a;
					var nKey = _v4.b;
					var nValue = _v4.c;
					var nLeft = _v4.d;
					var nRight = _v4.e;
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						$elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				$elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return $elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var $elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Black')) {
					var _v4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
						var _v6 = lLeft.a;
						return A5(
							$elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2($elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _v7 = $elm$core$Dict$moveRedLeft(dict);
						if (_v7.$ === 'RBNode_elm_builtin') {
							var nColor = _v7.a;
							var nKey = _v7.b;
							var nValue = _v7.c;
							var nLeft = _v7.d;
							var nRight = _v7.e;
							return A5(
								$elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2($elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return $elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						$elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2($elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					$elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7($elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var $elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBNode_elm_builtin') {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _v1 = $elm$core$Dict$getMin(right);
				if (_v1.$ === 'RBNode_elm_builtin') {
					var minKey = _v1.b;
					var minValue = _v1.c;
					return A5(
						$elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						$elm$core$Dict$removeMin(right));
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					$elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2($elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var $elm$core$Dict$remove = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$removeHelp, key, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _v0 = alter(
			A2($elm$core$Dict$get, targetKey, dictionary));
		if (_v0.$ === 'Just') {
			var value = _v0.a;
			return A3($elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2($elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var $elm$http$Http$emptyBody = _Http_emptyBody;
var $elm$json$Json$Decode$list = _Json_decodeList;
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $elm$json$Json$Decode$bool = _Json_decodeBool;
var $elm$json$Json$Decode$fail = _Json_fail;
var $elm$time$Time$Posix = function (a) {
	return {$: 'Posix', a: a};
};
var $elm$time$Time$millisToPosix = $elm$time$Time$Posix;
var $elm$core$String$replace = F3(
	function (before, after, string) {
		return A2(
			$elm$core$String$join,
			after,
			A2($elm$core$String$split, before, string));
	});
var $author$project$Supabase$decodeIsoTime = function (isoString) {
	var _v0 = $elm$core$String$toInt(
		A2(
			$elm$core$String$left,
			10,
			A3(
				$elm$core$String$replace,
				'-',
				'',
				A3($elm$core$String$replace, 'T', '', isoString))));
	if (_v0.$ === 'Just') {
		var timestamp = _v0.a;
		return $elm$json$Json$Decode$succeed(
			$elm$time$Time$millisToPosix(timestamp * 1000));
	} else {
		return $elm$json$Json$Decode$fail('Invalid ISO time: ' + isoString);
	}
};
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $elm$json$Json$Decode$map8 = _Json_map8;
var $elm$json$Json$Decode$string = _Json_decodeString;
var $author$project$Supabase$playerDecoder = A2(
	$elm$json$Json$Decode$andThen,
	function (partial) {
		return A2(
			$elm$json$Json$Decode$map,
			partial,
			A2(
				$elm$json$Json$Decode$field,
				'updated_at',
				A2($elm$json$Json$Decode$andThen, $author$project$Supabase$decodeIsoTime, $elm$json$Json$Decode$string)));
	},
	A9(
		$elm$json$Json$Decode$map8,
		F8(
			function (id, name, rating, matchesPlayed, playsAM, playsPM, isIgnored, createdAt) {
				return function (updatedAt) {
					return {createdAt: createdAt, id: id, isIgnored: isIgnored, matchesPlayed: matchesPlayed, name: name, playsAM: playsAM, playsPM: playsPM, rating: rating, updatedAt: updatedAt};
				};
			}),
		A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$int),
		A2($elm$json$Json$Decode$field, 'name', $elm$json$Json$Decode$string),
		A2($elm$json$Json$Decode$field, 'rating', $elm$json$Json$Decode$int),
		A2($elm$json$Json$Decode$field, 'matches_played', $elm$json$Json$Decode$int),
		A2($elm$json$Json$Decode$field, 'plays_am', $elm$json$Json$Decode$bool),
		A2($elm$json$Json$Decode$field, 'plays_pm', $elm$json$Json$Decode$bool),
		A2($elm$json$Json$Decode$field, 'is_ignored', $elm$json$Json$Decode$bool),
		A2(
			$elm$json$Json$Decode$field,
			'created_at',
			A2($elm$json$Json$Decode$andThen, $author$project$Supabase$decodeIsoTime, $elm$json$Json$Decode$string))));
var $elm$json$Json$Decode$decodeString = _Json_runOnString;
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $elm$http$Http$expectStringResponse = F2(
	function (toMsg, toResult) {
		return A3(
			_Http_expect,
			'',
			$elm$core$Basics$identity,
			A2($elm$core$Basics$composeR, toResult, toMsg));
	});
var $elm$core$Result$mapError = F2(
	function (f, result) {
		if (result.$ === 'Ok') {
			var v = result.a;
			return $elm$core$Result$Ok(v);
		} else {
			var e = result.a;
			return $elm$core$Result$Err(
				f(e));
		}
	});
var $elm$http$Http$BadBody = function (a) {
	return {$: 'BadBody', a: a};
};
var $elm$http$Http$BadStatus = function (a) {
	return {$: 'BadStatus', a: a};
};
var $elm$http$Http$BadUrl = function (a) {
	return {$: 'BadUrl', a: a};
};
var $elm$http$Http$NetworkError = {$: 'NetworkError'};
var $elm$http$Http$Timeout = {$: 'Timeout'};
var $elm$http$Http$resolve = F2(
	function (toResult, response) {
		switch (response.$) {
			case 'BadUrl_':
				var url = response.a;
				return $elm$core$Result$Err(
					$elm$http$Http$BadUrl(url));
			case 'Timeout_':
				return $elm$core$Result$Err($elm$http$Http$Timeout);
			case 'NetworkError_':
				return $elm$core$Result$Err($elm$http$Http$NetworkError);
			case 'BadStatus_':
				var metadata = response.a;
				return $elm$core$Result$Err(
					$elm$http$Http$BadStatus(metadata.statusCode));
			default:
				var body = response.b;
				return A2(
					$elm$core$Result$mapError,
					$elm$http$Http$BadBody,
					toResult(body));
		}
	});
var $elm$http$Http$expectJson = F2(
	function (toMsg, decoder) {
		return A2(
			$elm$http$Http$expectStringResponse,
			toMsg,
			$elm$http$Http$resolve(
				function (string) {
					return A2(
						$elm$core$Result$mapError,
						$elm$json$Json$Decode$errorToString,
						A2($elm$json$Json$Decode$decodeString, decoder, string));
				}));
	});
var $elm$http$Http$Header = F2(
	function (a, b) {
		return {$: 'Header', a: a, b: b};
	});
var $elm$http$Http$header = $elm$http$Http$Header;
var $elm$http$Http$Request = function (a) {
	return {$: 'Request', a: a};
};
var $elm$http$Http$State = F2(
	function (reqs, subs) {
		return {reqs: reqs, subs: subs};
	});
var $elm$http$Http$init = $elm$core$Task$succeed(
	A2($elm$http$Http$State, $elm$core$Dict$empty, _List_Nil));
var $elm$core$Process$kill = _Scheduler_kill;
var $elm$core$Process$spawn = _Scheduler_spawn;
var $elm$http$Http$updateReqs = F3(
	function (router, cmds, reqs) {
		updateReqs:
		while (true) {
			if (!cmds.b) {
				return $elm$core$Task$succeed(reqs);
			} else {
				var cmd = cmds.a;
				var otherCmds = cmds.b;
				if (cmd.$ === 'Cancel') {
					var tracker = cmd.a;
					var _v2 = A2($elm$core$Dict$get, tracker, reqs);
					if (_v2.$ === 'Nothing') {
						var $temp$router = router,
							$temp$cmds = otherCmds,
							$temp$reqs = reqs;
						router = $temp$router;
						cmds = $temp$cmds;
						reqs = $temp$reqs;
						continue updateReqs;
					} else {
						var pid = _v2.a;
						return A2(
							$elm$core$Task$andThen,
							function (_v3) {
								return A3(
									$elm$http$Http$updateReqs,
									router,
									otherCmds,
									A2($elm$core$Dict$remove, tracker, reqs));
							},
							$elm$core$Process$kill(pid));
					}
				} else {
					var req = cmd.a;
					return A2(
						$elm$core$Task$andThen,
						function (pid) {
							var _v4 = req.tracker;
							if (_v4.$ === 'Nothing') {
								return A3($elm$http$Http$updateReqs, router, otherCmds, reqs);
							} else {
								var tracker = _v4.a;
								return A3(
									$elm$http$Http$updateReqs,
									router,
									otherCmds,
									A3($elm$core$Dict$insert, tracker, pid, reqs));
							}
						},
						$elm$core$Process$spawn(
							A3(
								_Http_toTask,
								router,
								$elm$core$Platform$sendToApp(router),
								req)));
				}
			}
		}
	});
var $elm$http$Http$onEffects = F4(
	function (router, cmds, subs, state) {
		return A2(
			$elm$core$Task$andThen,
			function (reqs) {
				return $elm$core$Task$succeed(
					A2($elm$http$Http$State, reqs, subs));
			},
			A3($elm$http$Http$updateReqs, router, cmds, state.reqs));
	});
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (_v0.$ === 'Just') {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $elm$http$Http$maybeSend = F4(
	function (router, desiredTracker, progress, _v0) {
		var actualTracker = _v0.a;
		var toMsg = _v0.b;
		return _Utils_eq(desiredTracker, actualTracker) ? $elm$core$Maybe$Just(
			A2(
				$elm$core$Platform$sendToApp,
				router,
				toMsg(progress))) : $elm$core$Maybe$Nothing;
	});
var $elm$http$Http$onSelfMsg = F3(
	function (router, _v0, state) {
		var tracker = _v0.a;
		var progress = _v0.b;
		return A2(
			$elm$core$Task$andThen,
			function (_v1) {
				return $elm$core$Task$succeed(state);
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$filterMap,
					A3($elm$http$Http$maybeSend, router, tracker, progress),
					state.subs)));
	});
var $elm$http$Http$Cancel = function (a) {
	return {$: 'Cancel', a: a};
};
var $elm$http$Http$cmdMap = F2(
	function (func, cmd) {
		if (cmd.$ === 'Cancel') {
			var tracker = cmd.a;
			return $elm$http$Http$Cancel(tracker);
		} else {
			var r = cmd.a;
			return $elm$http$Http$Request(
				{
					allowCookiesFromOtherDomains: r.allowCookiesFromOtherDomains,
					body: r.body,
					expect: A2(_Http_mapExpect, func, r.expect),
					headers: r.headers,
					method: r.method,
					timeout: r.timeout,
					tracker: r.tracker,
					url: r.url
				});
		}
	});
var $elm$http$Http$MySub = F2(
	function (a, b) {
		return {$: 'MySub', a: a, b: b};
	});
var $elm$http$Http$subMap = F2(
	function (func, _v0) {
		var tracker = _v0.a;
		var toMsg = _v0.b;
		return A2(
			$elm$http$Http$MySub,
			tracker,
			A2($elm$core$Basics$composeR, toMsg, func));
	});
_Platform_effectManagers['Http'] = _Platform_createManager($elm$http$Http$init, $elm$http$Http$onEffects, $elm$http$Http$onSelfMsg, $elm$http$Http$cmdMap, $elm$http$Http$subMap);
var $elm$http$Http$command = _Platform_leaf('Http');
var $elm$http$Http$subscription = _Platform_leaf('Http');
var $elm$http$Http$request = function (r) {
	return $elm$http$Http$command(
		$elm$http$Http$Request(
			{allowCookiesFromOtherDomains: false, body: r.body, expect: r.expect, headers: r.headers, method: r.method, timeout: r.timeout, tracker: r.tracker, url: r.url}));
};
var $author$project$Supabase$supabaseRequest = F6(
	function (config, method, endpoint, body, decoder, toMsg) {
		return $elm$http$Http$request(
			{
				body: body,
				expect: A2($elm$http$Http$expectJson, toMsg, decoder),
				headers: _List_fromArray(
					[
						A2($elm$http$Http$header, 'apikey', config.anonKey),
						A2($elm$http$Http$header, 'Authorization', 'Bearer ' + config.anonKey),
						A2($elm$http$Http$header, 'Content-Type', 'application/json'),
						A2($elm$http$Http$header, 'Prefer', 'return=representation')
					]),
				method: method,
				timeout: $elm$core$Maybe$Nothing,
				tracker: $elm$core$Maybe$Nothing,
				url: config.url + ('/rest/v1' + endpoint)
			});
	});
var $author$project$Supabase$getPlayers = F2(
	function (config, toMsg) {
		return A6(
			$author$project$Supabase$supabaseRequest,
			config,
			'GET',
			'/players?order=rating.desc',
			$elm$http$Http$emptyBody,
			$elm$json$Json$Decode$list($author$project$Supabase$playerDecoder),
			toMsg);
	});
var $author$project$History$History = function (a) {
	return {$: 'History', a: a};
};
var $author$project$History$init = F2(
	function (retention, initial) {
		return $author$project$History$History(
			{current: initial, future: _List_Nil, past: _List_Nil, retention: retention});
	});
var $author$project$League$League = function (a) {
	return {$: 'League', a: a};
};
var $rtfeldman$elm_sorter_experiment$Internal$Dict$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var $rtfeldman$elm_sorter_experiment$Sort$Dict$empty = function (sorter) {
	return $rtfeldman$elm_sorter_experiment$Internal$Dict$Leaf(sorter);
};
var $rtfeldman$elm_sorter_experiment$Sort$Sorter = function (a) {
	return {$: 'Sorter', a: a};
};
var $rtfeldman$elm_sorter_experiment$Sort$by = F2(
	function (transform, _v0) {
		var sort = _v0.a;
		return $rtfeldman$elm_sorter_experiment$Sort$Sorter(
			F2(
				function (first, second) {
					return A2(
						sort,
						transform(first),
						transform(second));
				}));
	});
var $rtfeldman$elm_sorter_experiment$Sort$compareNumbers = F2(
	function (first, second) {
		return A2($elm$core$Basics$compare, first + 0, second);
	});
var $rtfeldman$elm_sorter_experiment$Sort$increasing = $rtfeldman$elm_sorter_experiment$Sort$Sorter($rtfeldman$elm_sorter_experiment$Sort$compareNumbers);
var $author$project$Player$idSorter = A2(
	$rtfeldman$elm_sorter_experiment$Sort$by,
	function (_v0) {
		var id_ = _v0.a;
		return id_;
	},
	$rtfeldman$elm_sorter_experiment$Sort$increasing);
var $author$project$League$init = $author$project$League$League(
	{
		currentMatch: $elm$core$Maybe$Nothing,
		ignored: _List_Nil,
		players: $rtfeldman$elm_sorter_experiment$Sort$Dict$empty($author$project$Player$idSorter)
	});
var $author$project$Main$GotNextMatch = function (a) {
	return {$: 'GotNextMatch', a: a};
};
var $author$project$History$current = function (_v0) {
	var guts = _v0.a;
	return guts.current;
};
var $author$project$League$currentMatch = function (_v0) {
	var league = _v0.a;
	return league.currentMatch;
};
var $elm$random$Random$Generate = function (a) {
	return {$: 'Generate', a: a};
};
var $elm$random$Random$Seed = F2(
	function (a, b) {
		return {$: 'Seed', a: a, b: b};
	});
var $elm$core$Bitwise$shiftRightZfBy = _Bitwise_shiftRightZfBy;
var $elm$random$Random$next = function (_v0) {
	var state0 = _v0.a;
	var incr = _v0.b;
	return A2($elm$random$Random$Seed, ((state0 * 1664525) + incr) >>> 0, incr);
};
var $elm$random$Random$initialSeed = function (x) {
	var _v0 = $elm$random$Random$next(
		A2($elm$random$Random$Seed, 0, 1013904223));
	var state1 = _v0.a;
	var incr = _v0.b;
	var state2 = (state1 + x) >>> 0;
	return $elm$random$Random$next(
		A2($elm$random$Random$Seed, state2, incr));
};
var $elm$time$Time$Name = function (a) {
	return {$: 'Name', a: a};
};
var $elm$time$Time$Offset = function (a) {
	return {$: 'Offset', a: a};
};
var $elm$time$Time$Zone = F2(
	function (a, b) {
		return {$: 'Zone', a: a, b: b};
	});
var $elm$time$Time$customZone = $elm$time$Time$Zone;
var $elm$time$Time$now = _Time_now($elm$time$Time$millisToPosix);
var $elm$time$Time$posixToMillis = function (_v0) {
	var millis = _v0.a;
	return millis;
};
var $elm$random$Random$init = A2(
	$elm$core$Task$andThen,
	function (time) {
		return $elm$core$Task$succeed(
			$elm$random$Random$initialSeed(
				$elm$time$Time$posixToMillis(time)));
	},
	$elm$time$Time$now);
var $elm$random$Random$step = F2(
	function (_v0, seed) {
		var generator = _v0.a;
		return generator(seed);
	});
var $elm$random$Random$onEffects = F3(
	function (router, commands, seed) {
		if (!commands.b) {
			return $elm$core$Task$succeed(seed);
		} else {
			var generator = commands.a.a;
			var rest = commands.b;
			var _v1 = A2($elm$random$Random$step, generator, seed);
			var value = _v1.a;
			var newSeed = _v1.b;
			return A2(
				$elm$core$Task$andThen,
				function (_v2) {
					return A3($elm$random$Random$onEffects, router, rest, newSeed);
				},
				A2($elm$core$Platform$sendToApp, router, value));
		}
	});
var $elm$random$Random$onSelfMsg = F3(
	function (_v0, _v1, seed) {
		return $elm$core$Task$succeed(seed);
	});
var $elm$random$Random$Generator = function (a) {
	return {$: 'Generator', a: a};
};
var $elm$random$Random$map = F2(
	function (func, _v0) {
		var genA = _v0.a;
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v1 = genA(seed0);
				var a = _v1.a;
				var seed1 = _v1.b;
				return _Utils_Tuple2(
					func(a),
					seed1);
			});
	});
var $elm$random$Random$cmdMap = F2(
	function (func, _v0) {
		var generator = _v0.a;
		return $elm$random$Random$Generate(
			A2($elm$random$Random$map, func, generator));
	});
_Platform_effectManagers['Random'] = _Platform_createManager($elm$random$Random$init, $elm$random$Random$onEffects, $elm$random$Random$onSelfMsg, $elm$random$Random$cmdMap);
var $elm$random$Random$command = _Platform_leaf('Random');
var $elm$random$Random$generate = F2(
	function (tagger, generator) {
		return $elm$random$Random$command(
			$elm$random$Random$Generate(
				A2($elm$random$Random$map, tagger, generator)));
	});
var $author$project$Player$id = function (_v0) {
	var player = _v0.a;
	return player.id;
};
var $elm$core$Dict$member = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$get, key, dict);
		if (_v0.$ === 'Just') {
			return true;
		} else {
			return false;
		}
	});
var $elm$core$Set$member = F2(
	function (key, _v0) {
		var dict = _v0.a;
		return A2($elm$core$Dict$member, key, dict);
	});
var $author$project$Main$isPlayerLocallyIgnored = F2(
	function (player, model) {
		var _v0 = $author$project$Player$id(player);
		var idInt = _v0.a;
		return A2(
			$elm$core$Set$member,
			$elm$core$String$fromInt(idInt),
			model.ignoredPlayers);
	});
var $elm$core$Basics$neq = _Utils_notEqual;
var $author$project$League$Match = F2(
	function (a, b) {
		return {$: 'Match', a: a, b: b};
	});
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$core$Basics$abs = function (n) {
	return (n < 0) ? (-n) : n;
};
var $elm$random$Random$andThen = F2(
	function (callback, _v0) {
		var genA = _v0.a;
		return $elm$random$Random$Generator(
			function (seed) {
				var _v1 = genA(seed);
				var result = _v1.a;
				var newSeed = _v1.b;
				var _v2 = callback(result);
				var genB = _v2.a;
				return genB(newSeed);
			});
	});
var $elm$random$Random$constant = function (value) {
	return $elm$random$Random$Generator(
		function (seed) {
			return _Utils_Tuple2(value, seed);
		});
};
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$Player$rating = function (_v0) {
	var player = _v0.a;
	return player.rating;
};
var $elm$core$List$sortBy = _List_sortBy;
var $rtfeldman$elm_sorter_experiment$Internal$Dict$foldr = F3(
	function (f, acc, dict) {
		foldr:
		while (true) {
			if (dict.$ === 'Leaf') {
				return acc;
			} else {
				var key = dict.c;
				var value = dict.d;
				var left = dict.e;
				var right = dict.f;
				var $temp$f = f,
					$temp$acc = A3(
					f,
					key,
					value,
					A3($rtfeldman$elm_sorter_experiment$Internal$Dict$foldr, f, acc, right)),
					$temp$dict = left;
				f = $temp$f;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldr;
			}
		}
	});
var $rtfeldman$elm_sorter_experiment$Sort$Dict$foldr = F3(
	function (f, acc, dict) {
		return A3($rtfeldman$elm_sorter_experiment$Internal$Dict$foldr, f, acc, dict);
	});
var $rtfeldman$elm_sorter_experiment$Sort$Dict$values = function (dict) {
	return A3(
		$rtfeldman$elm_sorter_experiment$Sort$Dict$foldr,
		F3(
			function (_v0, value, valueList) {
				return A2($elm$core$List$cons, value, valueList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $author$project$League$getPlayerRanking = F2(
	function (player, _v0) {
		var league = _v0.a;
		return A2(
			$elm$core$Maybe$withDefault,
			999,
			A2(
				$elm$core$Maybe$map,
				$elm$core$Tuple$first,
				$elm$core$List$head(
					A2(
						$elm$core$List$filter,
						function (_v1) {
							var p = _v1.b;
							return _Utils_eq(
								$author$project$Player$id(p),
								$author$project$Player$id(player));
						},
						A2(
							$elm$core$List$indexedMap,
							F2(
								function (index, p) {
									return _Utils_Tuple2(index + 1, p);
								}),
							A2(
								$elm$core$List$sortBy,
								function (p) {
									return -$author$project$Player$rating(p);
								},
								$rtfeldman$elm_sorter_experiment$Sort$Dict$values(league.players)))))));
	});
var $author$project$Player$matchesPlayed = function (_v0) {
	var player = _v0.a;
	return player.matches;
};
var $elm$core$List$maximum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(
			A3($elm$core$List$foldl, $elm$core$Basics$max, x, xs));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $elm$core$List$member = F2(
	function (x, xs) {
		return A2(
			$elm$core$List$any,
			function (a) {
				return _Utils_eq(a, x);
			},
			xs);
	});
var $elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var $elm$core$List$minimum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(
			A3($elm$core$List$foldl, $elm$core$Basics$min, x, xs));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$Basics$not = _Basics_not;
var $elm$core$Tuple$pair = F2(
	function (a, b) {
		return _Utils_Tuple2(a, b);
	});
var $author$project$League$playInMatches = 12;
var $elm$core$Basics$pow = _Basics_pow;
var $elm$random$Random$addOne = function (value) {
	return _Utils_Tuple2(1, value);
};
var $elm$core$Bitwise$and = _Bitwise_and;
var $elm$core$Bitwise$xor = _Bitwise_xor;
var $elm$random$Random$peel = function (_v0) {
	var state = _v0.a;
	var word = (state ^ (state >>> ((state >>> 28) + 4))) * 277803737;
	return ((word >>> 22) ^ word) >>> 0;
};
var $elm$random$Random$float = F2(
	function (a, b) {
		return $elm$random$Random$Generator(
			function (seed0) {
				var seed1 = $elm$random$Random$next(seed0);
				var range = $elm$core$Basics$abs(b - a);
				var n1 = $elm$random$Random$peel(seed1);
				var n0 = $elm$random$Random$peel(seed0);
				var lo = (134217727 & n1) * 1.0;
				var hi = (67108863 & n0) * 1.0;
				var val = ((hi * 134217728.0) + lo) / 9007199254740992.0;
				var scaled = (val * range) + a;
				return _Utils_Tuple2(
					scaled,
					$elm$random$Random$next(seed1));
			});
	});
var $elm$random$Random$getByWeight = F3(
	function (_v0, others, countdown) {
		getByWeight:
		while (true) {
			var weight = _v0.a;
			var value = _v0.b;
			if (!others.b) {
				return value;
			} else {
				var second = others.a;
				var otherOthers = others.b;
				if (_Utils_cmp(
					countdown,
					$elm$core$Basics$abs(weight)) < 1) {
					return value;
				} else {
					var $temp$_v0 = second,
						$temp$others = otherOthers,
						$temp$countdown = countdown - $elm$core$Basics$abs(weight);
					_v0 = $temp$_v0;
					others = $temp$others;
					countdown = $temp$countdown;
					continue getByWeight;
				}
			}
		}
	});
var $elm$core$List$sum = function (numbers) {
	return A3($elm$core$List$foldl, $elm$core$Basics$add, 0, numbers);
};
var $elm$random$Random$weighted = F2(
	function (first, others) {
		var normalize = function (_v0) {
			var weight = _v0.a;
			return $elm$core$Basics$abs(weight);
		};
		var total = normalize(first) + $elm$core$List$sum(
			A2($elm$core$List$map, normalize, others));
		return A2(
			$elm$random$Random$map,
			A2($elm$random$Random$getByWeight, first, others),
			A2($elm$random$Random$float, 0, total));
	});
var $elm$random$Random$uniform = F2(
	function (value, valueList) {
		return A2(
			$elm$random$Random$weighted,
			$elm$random$Random$addOne(value),
			A2($elm$core$List$map, $elm$random$Random$addOne, valueList));
	});
var $author$project$League$nextMatchFiltered = F2(
	function (allow, _v0) {
		var league = _v0.a;
		var allPlayersRaw = $rtfeldman$elm_sorter_experiment$Sort$Dict$values(league.players);
		var allPlayers = A2(
			$elm$core$List$filter,
			function (p) {
				return allow(p) && (!A2(
					$elm$core$List$member,
					$author$project$Player$id(p),
					league.ignored));
			},
			allPlayersRaw);
		if (allPlayers.b && allPlayers.b.b) {
			var a = allPlayers.a;
			var _v2 = allPlayers.b;
			var b = _v2.a;
			var rest = _v2.b;
			var _v3 = function () {
				var _v4 = A2(
					$elm$core$List$filter,
					function (player) {
						return _Utils_cmp(
							$author$project$Player$matchesPlayed(player),
							$author$project$League$playInMatches) < 1;
					},
					allPlayers);
				if (!_v4.b) {
					return _Utils_Tuple2(
						a,
						A2($elm$core$List$cons, b, rest));
				} else {
					var firstPlayIn = _v4.a;
					var restOfPlayIns = _v4.b;
					return _Utils_Tuple2(firstPlayIn, restOfPlayIns);
				}
			}();
			var firstPossiblePlayer = _v3.a;
			var restOfPossiblePlayers = _v3.b;
			var mostMatchesAmongPossiblePlayers = A2(
				$elm$core$Maybe$withDefault,
				$author$project$Player$matchesPlayed(firstPossiblePlayer),
				$elm$core$List$maximum(
					A2(
						$elm$core$List$map,
						$author$project$Player$matchesPlayed,
						A2($elm$core$List$cons, firstPossiblePlayer, restOfPossiblePlayers))));
			return A2(
				$elm$random$Random$map,
				$elm$core$Maybe$Just,
				A2(
					$elm$random$Random$andThen,
					function (_v8) {
						var playerA = _v8.a;
						var playerB = _v8.b;
						return A2(
							$elm$random$Random$map,
							function (flip) {
								return flip ? A2($author$project$League$Match, playerA, playerB) : A2($author$project$League$Match, playerB, playerA);
							},
							A2(
								$elm$random$Random$uniform,
								true,
								_List_fromArray(
									[false])));
					},
					A2(
						$elm$random$Random$andThen,
						function (firstPlayer) {
							var firstPlayerRanking = A2(
								$author$project$League$getPlayerRanking,
								firstPlayer,
								$author$project$League$League(league));
							var baseWeight = 10.0;
							var allOpponents = _Utils_eq(firstPlayer, a) ? A2($elm$core$List$cons, b, rest) : (_Utils_eq(firstPlayer, b) ? A2($elm$core$List$cons, a, rest) : A2(
								$elm$core$List$cons,
								a,
								A2(
									$elm$core$List$cons,
									b,
									A2(
										$elm$core$List$filter,
										function (p) {
											return !_Utils_eq(p, firstPlayer);
										},
										rest))));
							var eligibleOpponents = A2(
								$elm$core$List$filter,
								function (opponent) {
									var opponentRanking = A2(
										$author$project$League$getPlayerRanking,
										opponent,
										$author$project$League$League(league));
									return $elm$core$Basics$abs(firstPlayerRanking - opponentRanking) <= 10;
								},
								allOpponents);
							var _v5 = function () {
								if (eligibleOpponents.b) {
									var h = eligibleOpponents.a;
									var t = eligibleOpponents.b;
									return _Utils_Tuple2(h, t);
								} else {
									if (allOpponents.b) {
										var h = allOpponents.a;
										var t = allOpponents.b;
										return _Utils_Tuple2(h, t);
									} else {
										return _Utils_Tuple2(firstPlayer, _List_Nil);
									}
								}
							}();
							var head = _v5.a;
							var tail = _v5.b;
							var closestRatingDistance = A2(
								$elm$core$Maybe$withDefault,
								0,
								$elm$core$List$minimum(
									A2(
										$elm$core$List$map,
										function (player) {
											return $elm$core$Basics$abs(
												$author$project$Player$rating(firstPlayer) - $author$project$Player$rating(player));
										},
										A2($elm$core$List$cons, head, tail))));
							return A2(
								$elm$random$Random$map,
								$elm$core$Tuple$pair(firstPlayer),
								A2(
									$elm$random$Random$weighted,
									_Utils_Tuple2(
										baseWeight + (500 - A2(
											$elm$core$Basics$min,
											500,
											$elm$core$Basics$abs(
												$author$project$Player$rating(firstPlayer) - $author$project$Player$rating(head)))),
										head),
									A2(
										$elm$core$List$map,
										function (player) {
											return _Utils_Tuple2(
												baseWeight + (500 - A2(
													$elm$core$Basics$min,
													500,
													$elm$core$Basics$abs(
														$author$project$Player$rating(firstPlayer) - $author$project$Player$rating(player)))),
												player);
										},
										tail)));
						},
						A2(
							$elm$random$Random$weighted,
							_Utils_Tuple2(
								A2(
									$elm$core$Basics$pow,
									mostMatchesAmongPossiblePlayers - $author$project$Player$matchesPlayed(firstPossiblePlayer),
									2),
								firstPossiblePlayer),
							A2(
								$elm$core$List$map,
								function (player) {
									return _Utils_Tuple2(
										A2(
											$elm$core$Basics$pow,
											mostMatchesAmongPossiblePlayers - $author$project$Player$matchesPlayed(player),
											2),
										player);
								},
								restOfPossiblePlayers)))));
		} else {
			return $elm$random$Random$constant($elm$core$Maybe$Nothing);
		}
	});
var $author$project$Player$playsAM = function (_v0) {
	var player = _v0.a;
	return player.am;
};
var $author$project$Player$playsPM = function (_v0) {
	var player = _v0.a;
	return player.pm;
};
var $author$project$Main$timePlayerFilter = function (model) {
	return function (player) {
		var _v0 = model.timeFilter;
		switch (_v0.$) {
			case 'All':
				return true;
			case 'AMOnly':
				return $author$project$Player$playsAM(player);
			default:
				return $author$project$Player$playsPM(player);
		}
	};
};
var $author$project$Main$startNextMatchIfPossible = function (_v0) {
	var model = _v0.a;
	var cmd = _v0.b;
	return (!_Utils_eq(
		$author$project$League$currentMatch(
			$author$project$History$current(model.history)),
		$elm$core$Maybe$Nothing)) ? _Utils_Tuple2(model, cmd) : _Utils_Tuple2(
		model,
		$elm$core$Platform$Cmd$batch(
			_List_fromArray(
				[
					cmd,
					A2(
					$elm$random$Random$generate,
					$author$project$Main$GotNextMatch,
					A2(
						$author$project$League$nextMatchFiltered,
						function (player) {
							return (!A2($author$project$Main$isPlayerLocallyIgnored, player, model)) && A2($author$project$Main$timePlayerFilter, model, player);
						},
						$author$project$History$current(model.history)))
				])));
};
var $author$project$Config$supabaseConfig = {anonKey: 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6InF6dnFibmFpeHBldnVpc3FheGt5Iiwicm9sZSI6ImFub24iLCJpYXQiOjE3NTk4NjU2NTMsImV4cCI6MjA3NTQ0MTY1M30.ZSX2dCWhTQQCG2v6aCI7gojUUOXaLrobP_VVQ-HDZjM', url: 'https://qzvqbnaixpevuisqaxky.supabase.co'};
var $author$project$Main$init = function (_v0) {
	return $author$project$Main$startNextMatchIfPossible(
		_Utils_Tuple2(
			{
				addPlayerAM: true,
				addPlayerName: '',
				addPlayerPM: true,
				addPlayerRating: '500',
				autoSave: true,
				autoSaveInProgress: false,
				customMatchupPlayerA: $elm$core$Maybe$Nothing,
				customMatchupPlayerB: $elm$core$Maybe$Nothing,
				dataVersion: 1,
				history: A2($author$project$History$init, 50, $author$project$League$init),
				ignoredPlayers: $elm$core$Set$empty,
				isStatusTemporary: false,
				isSyncing: false,
				lastModified: 0,
				lastSynced: $elm$core$Maybe$Nothing,
				newPlayerName: '',
				pendingMatch: $elm$core$Maybe$Nothing,
				playerASearch: '',
				playerASearchResults: _List_Nil,
				playerBSearch: '',
				playerBSearchResults: _List_Nil,
				playerDeletionConfirmation: $elm$core$Maybe$Nothing,
				shouldStartNextMatchAfterLoad: false,
				showAddPlayerPopup: false,
				showCustomMatchup: false,
				status: $elm$core$Maybe$Nothing,
				timeFilter: $author$project$Main$All,
				timeToggleConfirmation: $elm$core$Maybe$Nothing,
				votesSinceLastSync: 0
			},
			$elm$core$Platform$Cmd$batch(
				_List_fromArray(
					[
						$author$project$Main$askForAutoSave('init'),
						$author$project$Main$askForTimeFilter('init'),
						$author$project$Main$askForIgnoredPlayers('init'),
						A2($author$project$Supabase$getPlayers, $author$project$Config$supabaseConfig, $author$project$Main$GotPlayers)
					]))));
};
var $author$project$Main$ReceivedAutoSave = function (a) {
	return {$: 'ReceivedAutoSave', a: a};
};
var $author$project$Main$ReceivedIgnoredPlayers = function (a) {
	return {$: 'ReceivedIgnoredPlayers', a: a};
};
var $author$project$Main$ReceivedTimeFilter = function (a) {
	return {$: 'ReceivedTimeFilter', a: a};
};
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $author$project$Main$KeyPressed = function (a) {
	return {$: 'KeyPressed', a: a};
};
var $author$project$Main$keyDecoder = A2(
	$elm$json$Json$Decode$map,
	$author$project$Main$KeyPressed,
	A2($elm$json$Json$Decode$field, 'key', $elm$json$Json$Decode$string));
var $elm$browser$Browser$Events$Document = {$: 'Document'};
var $elm$browser$Browser$Events$MySub = F3(
	function (a, b, c) {
		return {$: 'MySub', a: a, b: b, c: c};
	});
var $elm$browser$Browser$Events$State = F2(
	function (subs, pids) {
		return {pids: pids, subs: subs};
	});
var $elm$browser$Browser$Events$init = $elm$core$Task$succeed(
	A2($elm$browser$Browser$Events$State, _List_Nil, $elm$core$Dict$empty));
var $elm$browser$Browser$Events$nodeToKey = function (node) {
	if (node.$ === 'Document') {
		return 'd_';
	} else {
		return 'w_';
	}
};
var $elm$browser$Browser$Events$addKey = function (sub) {
	var node = sub.a;
	var name = sub.b;
	return _Utils_Tuple2(
		_Utils_ap(
			$elm$browser$Browser$Events$nodeToKey(node),
			name),
		sub);
};
var $elm$core$Dict$fromList = function (assocs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, dict) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($elm$core$Dict$insert, key, value, dict);
			}),
		$elm$core$Dict$empty,
		assocs);
};
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _v0) {
				stepState:
				while (true) {
					var list = _v0.a;
					var result = _v0.b;
					if (!list.b) {
						return _Utils_Tuple2(
							list,
							A3(rightStep, rKey, rValue, result));
					} else {
						var _v2 = list.a;
						var lKey = _v2.a;
						var lValue = _v2.b;
						var rest = list.b;
						if (_Utils_cmp(lKey, rKey) < 0) {
							var $temp$rKey = rKey,
								$temp$rValue = rValue,
								$temp$_v0 = _Utils_Tuple2(
								rest,
								A3(leftStep, lKey, lValue, result));
							rKey = $temp$rKey;
							rValue = $temp$rValue;
							_v0 = $temp$_v0;
							continue stepState;
						} else {
							if (_Utils_cmp(lKey, rKey) > 0) {
								return _Utils_Tuple2(
									list,
									A3(rightStep, rKey, rValue, result));
							} else {
								return _Utils_Tuple2(
									rest,
									A4(bothStep, lKey, lValue, rValue, result));
							}
						}
					}
				}
			});
		var _v3 = A3(
			$elm$core$Dict$foldl,
			stepState,
			_Utils_Tuple2(
				$elm$core$Dict$toList(leftDict),
				initialResult),
			rightDict);
		var leftovers = _v3.a;
		var intermediateResult = _v3.b;
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_v4, result) {
					var k = _v4.a;
					var v = _v4.b;
					return A3(leftStep, k, v, result);
				}),
			intermediateResult,
			leftovers);
	});
var $elm$browser$Browser$Events$Event = F2(
	function (key, event) {
		return {event: event, key: key};
	});
var $elm$browser$Browser$Events$spawn = F3(
	function (router, key, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var actualNode = function () {
			if (node.$ === 'Document') {
				return _Browser_doc;
			} else {
				return _Browser_window;
			}
		}();
		return A2(
			$elm$core$Task$map,
			function (value) {
				return _Utils_Tuple2(key, value);
			},
			A3(
				_Browser_on,
				actualNode,
				name,
				function (event) {
					return A2(
						$elm$core$Platform$sendToSelf,
						router,
						A2($elm$browser$Browser$Events$Event, key, event));
				}));
	});
var $elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3($elm$core$Dict$foldl, $elm$core$Dict$insert, t2, t1);
	});
var $elm$browser$Browser$Events$onEffects = F3(
	function (router, subs, state) {
		var stepRight = F3(
			function (key, sub, _v6) {
				var deads = _v6.a;
				var lives = _v6.b;
				var news = _v6.c;
				return _Utils_Tuple3(
					deads,
					lives,
					A2(
						$elm$core$List$cons,
						A3($elm$browser$Browser$Events$spawn, router, key, sub),
						news));
			});
		var stepLeft = F3(
			function (_v4, pid, _v5) {
				var deads = _v5.a;
				var lives = _v5.b;
				var news = _v5.c;
				return _Utils_Tuple3(
					A2($elm$core$List$cons, pid, deads),
					lives,
					news);
			});
		var stepBoth = F4(
			function (key, pid, _v2, _v3) {
				var deads = _v3.a;
				var lives = _v3.b;
				var news = _v3.c;
				return _Utils_Tuple3(
					deads,
					A3($elm$core$Dict$insert, key, pid, lives),
					news);
			});
		var newSubs = A2($elm$core$List$map, $elm$browser$Browser$Events$addKey, subs);
		var _v0 = A6(
			$elm$core$Dict$merge,
			stepLeft,
			stepBoth,
			stepRight,
			state.pids,
			$elm$core$Dict$fromList(newSubs),
			_Utils_Tuple3(_List_Nil, $elm$core$Dict$empty, _List_Nil));
		var deadPids = _v0.a;
		var livePids = _v0.b;
		var makeNewPids = _v0.c;
		return A2(
			$elm$core$Task$andThen,
			function (pids) {
				return $elm$core$Task$succeed(
					A2(
						$elm$browser$Browser$Events$State,
						newSubs,
						A2(
							$elm$core$Dict$union,
							livePids,
							$elm$core$Dict$fromList(pids))));
			},
			A2(
				$elm$core$Task$andThen,
				function (_v1) {
					return $elm$core$Task$sequence(makeNewPids);
				},
				$elm$core$Task$sequence(
					A2($elm$core$List$map, $elm$core$Process$kill, deadPids))));
	});
var $elm$browser$Browser$Events$onSelfMsg = F3(
	function (router, _v0, state) {
		var key = _v0.key;
		var event = _v0.event;
		var toMessage = function (_v2) {
			var subKey = _v2.a;
			var _v3 = _v2.b;
			var node = _v3.a;
			var name = _v3.b;
			var decoder = _v3.c;
			return _Utils_eq(subKey, key) ? A2(_Browser_decodeEvent, decoder, event) : $elm$core$Maybe$Nothing;
		};
		var messages = A2($elm$core$List$filterMap, toMessage, state.subs);
		return A2(
			$elm$core$Task$andThen,
			function (_v1) {
				return $elm$core$Task$succeed(state);
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Platform$sendToApp(router),
					messages)));
	});
var $elm$browser$Browser$Events$subMap = F2(
	function (func, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var decoder = _v0.c;
		return A3(
			$elm$browser$Browser$Events$MySub,
			node,
			name,
			A2($elm$json$Json$Decode$map, func, decoder));
	});
_Platform_effectManagers['Browser.Events'] = _Platform_createManager($elm$browser$Browser$Events$init, $elm$browser$Browser$Events$onEffects, $elm$browser$Browser$Events$onSelfMsg, 0, $elm$browser$Browser$Events$subMap);
var $elm$browser$Browser$Events$subscription = _Platform_leaf('Browser.Events');
var $elm$browser$Browser$Events$on = F3(
	function (node, name, decoder) {
		return $elm$browser$Browser$Events$subscription(
			A3($elm$browser$Browser$Events$MySub, node, name, decoder));
	});
var $elm$browser$Browser$Events$onKeyDown = A2($elm$browser$Browser$Events$on, $elm$browser$Browser$Events$Document, 'keydown');
var $author$project$Main$receiveAutoSave = _Platform_incomingPort('receiveAutoSave', $elm$json$Json$Decode$bool);
var $author$project$Main$receiveIgnoredPlayers = _Platform_incomingPort('receiveIgnoredPlayers', $elm$json$Json$Decode$string);
var $author$project$Main$receiveTimeFilter = _Platform_incomingPort('receiveTimeFilter', $elm$json$Json$Decode$string);
var $author$project$Main$subscriptions = function (model) {
	return $elm$core$Platform$Sub$batch(
		_List_fromArray(
			[
				$author$project$Main$receiveAutoSave($author$project$Main$ReceivedAutoSave),
				$author$project$Main$receiveTimeFilter($author$project$Main$ReceivedTimeFilter),
				$author$project$Main$receiveIgnoredPlayers($author$project$Main$ReceivedIgnoredPlayers),
				$elm$browser$Browser$Events$onKeyDown($author$project$Main$keyDecoder)
			]));
};
var $author$project$League$Draw = function (a) {
	return {$: 'Draw', a: a};
};
var $author$project$Main$NewPlayerCreated = function (a) {
	return {$: 'NewPlayerCreated', a: a};
};
var $author$project$Main$PlayerDeleted = function (a) {
	return {$: 'PlayerDeleted', a: a};
};
var $author$project$Main$ShowStatus = function (a) {
	return {$: 'ShowStatus', a: a};
};
var $author$project$Main$TriggerReload = {$: 'TriggerReload'};
var $author$project$League$Win = function (a) {
	return {$: 'Win', a: a};
};
var $rtfeldman$elm_sorter_experiment$Internal$Dict$Black = {$: 'Black'};
var $rtfeldman$elm_sorter_experiment$Internal$Dict$Node = F6(
	function (a, b, c, d, e, f) {
		return {$: 'Node', a: a, b: b, c: c, d: d, e: e, f: f};
	});
var $rtfeldman$elm_sorter_experiment$Internal$Dict$Red = {$: 'Red'};
var $rtfeldman$elm_sorter_experiment$Sort$Dict$balance = F6(
	function (sorter, color, key, value, left, right) {
		if ((right.$ === 'Node') && (right.b.$ === 'Red')) {
			var _v1 = right.b;
			var rK = right.c;
			var rV = right.d;
			var rLeft = right.e;
			var rRight = right.f;
			if ((left.$ === 'Node') && (left.b.$ === 'Red')) {
				var _v3 = left.b;
				var lK = left.c;
				var lV = left.d;
				var lLeft = left.e;
				var lRight = left.f;
				return A6(
					$rtfeldman$elm_sorter_experiment$Internal$Dict$Node,
					sorter,
					$rtfeldman$elm_sorter_experiment$Internal$Dict$Red,
					key,
					value,
					A6($rtfeldman$elm_sorter_experiment$Internal$Dict$Node, sorter, $rtfeldman$elm_sorter_experiment$Internal$Dict$Black, lK, lV, lLeft, lRight),
					A6($rtfeldman$elm_sorter_experiment$Internal$Dict$Node, sorter, $rtfeldman$elm_sorter_experiment$Internal$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A6(
					$rtfeldman$elm_sorter_experiment$Internal$Dict$Node,
					sorter,
					color,
					rK,
					rV,
					A6($rtfeldman$elm_sorter_experiment$Internal$Dict$Node, sorter, $rtfeldman$elm_sorter_experiment$Internal$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'Node') && (left.b.$ === 'Red')) && (left.e.$ === 'Node')) && (left.e.b.$ === 'Red')) {
				var _v5 = left.b;
				var lK = left.c;
				var lV = left.d;
				var _v6 = left.e;
				var _v7 = _v6.b;
				var llK = _v6.c;
				var llV = _v6.d;
				var llLeft = _v6.e;
				var llRight = _v6.f;
				var lRight = left.f;
				return A6(
					$rtfeldman$elm_sorter_experiment$Internal$Dict$Node,
					sorter,
					$rtfeldman$elm_sorter_experiment$Internal$Dict$Red,
					lK,
					lV,
					A6($rtfeldman$elm_sorter_experiment$Internal$Dict$Node, sorter, $rtfeldman$elm_sorter_experiment$Internal$Dict$Black, llK, llV, llLeft, llRight),
					A6($rtfeldman$elm_sorter_experiment$Internal$Dict$Node, sorter, $rtfeldman$elm_sorter_experiment$Internal$Dict$Black, key, value, lRight, right));
			} else {
				return A6($rtfeldman$elm_sorter_experiment$Internal$Dict$Node, sorter, color, key, value, left, right);
			}
		}
	});
var $rtfeldman$elm_sorter_experiment$Sort$toOrder = F3(
	function (_v0, first, second) {
		var sort = _v0.a;
		return A2(sort, first, second);
	});
var $rtfeldman$elm_sorter_experiment$Sort$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'Leaf') {
			var sorter = dict.a;
			return A6(
				$rtfeldman$elm_sorter_experiment$Internal$Dict$Node,
				sorter,
				$rtfeldman$elm_sorter_experiment$Internal$Dict$Red,
				key,
				value,
				$rtfeldman$elm_sorter_experiment$Internal$Dict$Leaf(sorter),
				$rtfeldman$elm_sorter_experiment$Internal$Dict$Leaf(sorter));
		} else {
			var sorter = dict.a;
			var nColor = dict.b;
			var nKey = dict.c;
			var nValue = dict.d;
			var nLeft = dict.e;
			var nRight = dict.f;
			var _v1 = A3($rtfeldman$elm_sorter_experiment$Sort$toOrder, sorter, key, nKey);
			switch (_v1.$) {
				case 'LT':
					return A6(
						$rtfeldman$elm_sorter_experiment$Sort$Dict$balance,
						sorter,
						nColor,
						nKey,
						nValue,
						A3($rtfeldman$elm_sorter_experiment$Sort$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'GT':
					return A6(
						$rtfeldman$elm_sorter_experiment$Sort$Dict$balance,
						sorter,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($rtfeldman$elm_sorter_experiment$Sort$Dict$insertHelp, key, value, nRight));
				default:
					return A6($rtfeldman$elm_sorter_experiment$Internal$Dict$Node, sorter, nColor, nKey, value, nLeft, nRight);
			}
		}
	});
var $rtfeldman$elm_sorter_experiment$Sort$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($rtfeldman$elm_sorter_experiment$Sort$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === 'Node') && (_v0.b.$ === 'Red')) {
			var sorter = _v0.a;
			var _v1 = _v0.b;
			var k = _v0.c;
			var v = _v0.d;
			var l = _v0.e;
			var r = _v0.f;
			return A6($rtfeldman$elm_sorter_experiment$Internal$Dict$Node, sorter, $rtfeldman$elm_sorter_experiment$Internal$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $author$project$League$addPlayer = F2(
	function (player, _v0) {
		var league = _v0.a;
		return $author$project$League$League(
			_Utils_update(
				league,
				{
					players: A3(
						$rtfeldman$elm_sorter_experiment$Sort$Dict$insert,
						$author$project$Player$id(player),
						player,
						league.players)
				}));
	});
var $author$project$League$clearMatch = function (_v0) {
	var league = _v0.a;
	return $author$project$League$League(
		_Utils_update(
			league,
			{currentMatch: $elm$core$Maybe$Nothing}));
};
var $elm$json$Json$Encode$bool = _Json_wrap;
var $elm$core$Basics$modBy = _Basics_modBy;
var $elm$core$String$cons = _String_cons;
var $elm$core$String$fromChar = function (_char) {
	return A2($elm$core$String$cons, _char, '');
};
var $elm$core$Bitwise$shiftRightBy = _Bitwise_shiftRightBy;
var $elm$core$String$repeatHelp = F3(
	function (n, chunk, result) {
		return (n <= 0) ? result : A3(
			$elm$core$String$repeatHelp,
			n >> 1,
			_Utils_ap(chunk, chunk),
			(!(n & 1)) ? result : _Utils_ap(result, chunk));
	});
var $elm$core$String$repeat = F2(
	function (n, chunk) {
		return A3($elm$core$String$repeatHelp, n, chunk, '');
	});
var $elm$core$String$padLeft = F3(
	function (n, _char, string) {
		return _Utils_ap(
			A2(
				$elm$core$String$repeat,
				n - $elm$core$String$length(string),
				$elm$core$String$fromChar(_char)),
			string);
	});
var $author$project$Supabase$encodeIsoTime = function (time) {
	var year = 2025;
	var month = 10;
	var millis = $elm$time$Time$posixToMillis(time);
	var totalSeconds = (millis / 1000) | 0;
	var minute = A2($elm$core$Basics$modBy, 60, (totalSeconds / 60) | 0);
	var second = A2($elm$core$Basics$modBy, 60, totalSeconds);
	var hour = A2($elm$core$Basics$modBy, 24, (totalSeconds / 3600) | 0);
	var days = (totalSeconds / 86400) | 0;
	var day = 8;
	return $elm$core$String$fromInt(year) + ('-' + (A3(
		$elm$core$String$padLeft,
		2,
		_Utils_chr('0'),
		$elm$core$String$fromInt(month)) + ('-' + (A3(
		$elm$core$String$padLeft,
		2,
		_Utils_chr('0'),
		$elm$core$String$fromInt(day)) + ('T' + (A3(
		$elm$core$String$padLeft,
		2,
		_Utils_chr('0'),
		$elm$core$String$fromInt(hour)) + (':' + (A3(
		$elm$core$String$padLeft,
		2,
		_Utils_chr('0'),
		$elm$core$String$fromInt(minute)) + (':' + (A3(
		$elm$core$String$padLeft,
		2,
		_Utils_chr('0'),
		$elm$core$String$fromInt(second)) + 'Z'))))))))));
};
var $elm$json$Json$Decode$index = _Json_decodeIndex;
var $elm$json$Json$Encode$int = _Json_wrap;
var $elm$http$Http$jsonBody = function (value) {
	return A2(
		_Http_pair,
		'application/json',
		A2($elm$json$Json$Encode$encode, 0, value));
};
var $elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, obj) {
					var k = _v0.a;
					var v = _v0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(_Utils_Tuple0),
			pairs));
};
var $author$project$Supabase$createNewPlayer = F6(
	function (config, name, rating, playsAM, playsPM, toMsg) {
		var now = $elm$time$Time$millisToPosix(1728396000000);
		var playerData = $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'name',
					$elm$json$Json$Encode$string(name)),
					_Utils_Tuple2(
					'rating',
					$elm$json$Json$Encode$int(rating)),
					_Utils_Tuple2(
					'matches_played',
					$elm$json$Json$Encode$int(0)),
					_Utils_Tuple2(
					'plays_am',
					$elm$json$Json$Encode$bool(playsAM)),
					_Utils_Tuple2(
					'plays_pm',
					$elm$json$Json$Encode$bool(playsPM)),
					_Utils_Tuple2(
					'is_ignored',
					$elm$json$Json$Encode$bool(false)),
					_Utils_Tuple2(
					'created_at',
					$elm$json$Json$Encode$string(
						$author$project$Supabase$encodeIsoTime(now))),
					_Utils_Tuple2(
					'updated_at',
					$elm$json$Json$Encode$string(
						$author$project$Supabase$encodeIsoTime(now)))
				]));
		return $elm$http$Http$request(
			{
				body: $elm$http$Http$jsonBody(playerData),
				expect: A2(
					$elm$http$Http$expectJson,
					toMsg,
					A2($elm$json$Json$Decode$index, 0, $author$project$Supabase$playerDecoder)),
				headers: _List_fromArray(
					[
						A2($elm$http$Http$header, 'apikey', config.anonKey),
						A2($elm$http$Http$header, 'Authorization', 'Bearer ' + config.anonKey),
						A2($elm$http$Http$header, 'Content-Type', 'application/json'),
						A2($elm$http$Http$header, 'Prefer', 'return=representation')
					]),
				method: 'POST',
				timeout: $elm$core$Maybe$Nothing,
				tracker: $elm$core$Maybe$Nothing,
				url: config.url + '/rest/v1/players'
			});
	});
var $author$project$Supabase$deletePlayer = F3(
	function (config, playerId, toMsg) {
		return $elm$http$Http$request(
			{
				body: $elm$http$Http$emptyBody,
				expect: A2(
					$elm$http$Http$expectStringResponse,
					toMsg,
					function (response) {
						switch (response.$) {
							case 'GoodStatus_':
								return $elm$core$Result$Ok(_Utils_Tuple0);
							case 'BadStatus_':
								var metadata = response.a;
								var body = response.b;
								return $elm$core$Result$Err(
									$elm$http$Http$BadStatus(metadata.statusCode));
							case 'BadUrl_':
								var url = response.a;
								return $elm$core$Result$Err(
									$elm$http$Http$BadUrl(url));
							case 'Timeout_':
								return $elm$core$Result$Err($elm$http$Http$Timeout);
							default:
								return $elm$core$Result$Err($elm$http$Http$NetworkError);
						}
					}),
				headers: _List_fromArray(
					[
						A2($elm$http$Http$header, 'apikey', config.anonKey),
						A2($elm$http$Http$header, 'Authorization', 'Bearer ' + config.anonKey),
						A2($elm$http$Http$header, 'Prefer', 'return=minimal')
					]),
				method: 'DELETE',
				timeout: $elm$core$Maybe$Nothing,
				tracker: $elm$core$Maybe$Nothing,
				url: config.url + ('/rest/v1/players?id=eq.' + $elm$core$String$fromInt(playerId))
			});
	});
var $elm$core$Set$insert = F2(
	function (key, _v0) {
		var dict = _v0.a;
		return $elm$core$Set$Set_elm_builtin(
			A3($elm$core$Dict$insert, key, _Utils_Tuple0, dict));
	});
var $elm$core$Set$fromList = function (list) {
	return A3($elm$core$List$foldl, $elm$core$Set$insert, $elm$core$Set$empty, list);
};
var $elm$core$Basics$ge = _Utils_ge;
var $rtfeldman$elm_sorter_experiment$Sort$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'Leaf') {
				return $elm$core$Maybe$Nothing;
			} else {
				var sorter = dict.a;
				var key = dict.c;
				var value = dict.d;
				var left = dict.e;
				var right = dict.f;
				var _v1 = A3($rtfeldman$elm_sorter_experiment$Sort$toOrder, sorter, targetKey, key);
				switch (_v1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'GT':
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					default:
						return $elm$core$Maybe$Just(value);
				}
			}
		}
	});
var $author$project$League$getPlayer = F2(
	function (id, _v0) {
		var league = _v0.a;
		return A2($rtfeldman$elm_sorter_experiment$Sort$Dict$get, id, league.players);
	});
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $author$project$History$goBack = function (_v0) {
	var guts = _v0.a;
	var _v1 = guts.past;
	if (_v1.b) {
		var mostRecent = _v1.a;
		var rest = _v1.b;
		return A2($elm$core$Basics$composeL, $elm$core$Maybe$Just, $author$project$History$History)(
			_Utils_update(
				guts,
				{
					current: mostRecent,
					future: A2($elm$core$List$cons, guts.current, guts.future),
					past: rest
				}));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$History$goForward = function (_v0) {
	var guts = _v0.a;
	var _v1 = guts.future;
	if (_v1.b) {
		var nextRecent = _v1.a;
		var rest = _v1.b;
		return A2($elm$core$Basics$composeL, $elm$core$Maybe$Just, $author$project$History$History)(
			_Utils_update(
				guts,
				{
					current: nextRecent,
					future: rest,
					past: A2($elm$core$List$cons, guts.current, guts.past)
				}));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Main$MatchSaved = function (a) {
	return {$: 'MatchSaved', a: a};
};
var $author$project$Elo$odds = F2(
	function (a, b) {
		var rB = A2($elm$core$Basics$pow, 10, b / 600);
		var rA = A2($elm$core$Basics$pow, 10, a / 600);
		var rawOdds = rA / (rA + rB);
		var minOdds = 0.15;
		var maxOdds = 0.85;
		return (_Utils_cmp(rawOdds, minOdds) < 0) ? minOdds : ((_Utils_cmp(rawOdds, maxOdds) > 0) ? maxOdds : rawOdds);
	});
var $elm$core$Basics$round = _Basics_round;
var $author$project$Elo$draw = F2(
	function (kFactor, _v0) {
		var playerA = _v0.playerA;
		var playerB = _v0.playerB;
		return {
			playerA: $elm$core$Basics$round(
				playerA + (kFactor * (0.5 - A2($author$project$Elo$odds, playerA, playerB)))),
			playerB: $elm$core$Basics$round(
				playerB + (kFactor * (0.5 - A2($author$project$Elo$odds, playerB, playerA))))
		};
	});
var $author$project$League$higherRankedPlayer = F2(
	function (a, b) {
		return (_Utils_cmp(
			$author$project$Player$rating(a),
			$author$project$Player$rating(b)) > 0) ? a : b;
	});
var $author$project$Elo$dynamicKFactor = F2(
	function (gamesPlayed, currentRating) {
		return (gamesPlayed <= 20) ? 24 : ((gamesPlayed <= 50) ? 16 : ((currentRating >= 800) ? 8 : 12));
	});
var $author$project$Elo$getKFactor = F2(
	function (gamesPlayed, currentRating) {
		return A2($author$project$Elo$dynamicKFactor, gamesPlayed, currentRating);
	});
var $author$project$League$kFactor = F2(
	function (_v0, player) {
		var league = _v0.a;
		return A2(
			$author$project$Elo$getKFactor,
			$author$project$Player$matchesPlayed(player),
			$author$project$Player$rating(player));
	});
var $author$project$League$updatePlayer = F2(
	function (player, _v0) {
		var league = _v0.a;
		return $author$project$League$League(
			_Utils_update(
				league,
				{
					players: A3(
						$rtfeldman$elm_sorter_experiment$Sort$Dict$insert,
						$author$project$Player$id(player),
						player,
						league.players)
				}));
	});
var $author$project$Player$Player = function (a) {
	return {$: 'Player', a: a};
};
var $author$project$Player$incrementMatchesPlayed = function (_v0) {
	var player = _v0.a;
	return $author$project$Player$Player(
		_Utils_update(
			player,
			{matches: player.matches + 1}));
};
var $author$project$Player$setRating = F2(
	function (rating_, _v0) {
		var player = _v0.a;
		return $author$project$Player$Player(
			_Utils_update(
				player,
				{
					rating: A2($elm$core$Basics$max, 0, rating_)
				}));
	});
var $author$project$League$updateRatingsIncludingPlayInPeriod = F2(
	function (ratings, players_) {
		var playerBInPlayInPeriod = _Utils_cmp(
			$author$project$Player$matchesPlayed(players_.playerB),
			$author$project$League$playInMatches) < 0;
		var playerAInPlayInPeriod = _Utils_cmp(
			$author$project$Player$matchesPlayed(players_.playerA),
			$author$project$League$playInMatches) < 0;
		return {
			playerA: ((!playerAInPlayInPeriod) && playerBInPlayInPeriod) ? players_.playerA : $author$project$Player$incrementMatchesPlayed(
				A2($author$project$Player$setRating, ratings.playerA, players_.playerA)),
			playerB: ((!playerBInPlayInPeriod) && playerAInPlayInPeriod) ? players_.playerB : $author$project$Player$incrementMatchesPlayed(
				A2($author$project$Player$setRating, ratings.playerB, players_.playerB))
		};
	});
var $author$project$Elo$win = F2(
	function (kFactor, _v0) {
		var won = _v0.won;
		var lost = _v0.lost;
		return {
			lost: $elm$core$Basics$round(
				lost + (kFactor * (0 - A2($author$project$Elo$odds, lost, won)))),
			won: $elm$core$Basics$round(
				won + (kFactor * (1 - A2($author$project$Elo$odds, won, lost))))
		};
	});
var $author$project$League$finishMatch = F2(
	function (outcome, league) {
		if (outcome.$ === 'Win') {
			var won = outcome.a.won;
			var lost = outcome.a.lost;
			var newRatings = A2(
				$author$project$Elo$win,
				A2($author$project$League$kFactor, league, won),
				{
					lost: $author$project$Player$rating(lost),
					won: $author$project$Player$rating(won)
				});
			var newPlayers = A2(
				$author$project$League$updateRatingsIncludingPlayInPeriod,
				{playerA: newRatings.won, playerB: newRatings.lost},
				{playerA: won, playerB: lost});
			return $author$project$League$clearMatch(
				A2(
					$author$project$League$updatePlayer,
					newPlayers.playerB,
					A2($author$project$League$updatePlayer, newPlayers.playerA, league)));
		} else {
			var playerA = outcome.a.playerA;
			var playerB = outcome.a.playerB;
			var newRatings = A2(
				$author$project$Elo$draw,
				A2(
					$author$project$League$kFactor,
					league,
					A2($author$project$League$higherRankedPlayer, playerA, playerB)),
				{
					playerA: $author$project$Player$rating(playerA),
					playerB: $author$project$Player$rating(playerB)
				});
			var newPlayers = A2(
				$author$project$League$updateRatingsIncludingPlayInPeriod,
				newRatings,
				{playerA: playerA, playerB: playerB});
			return $author$project$League$clearMatch(
				A2(
					$author$project$League$updatePlayer,
					newPlayers.playerB,
					A2($author$project$League$updatePlayer, newPlayers.playerA, league)));
		}
	});
var $author$project$Main$isVotingDisabled = function (model) {
	return model.autoSaveInProgress || model.isSyncing;
};
var $elm$core$List$takeReverse = F3(
	function (n, list, kept) {
		takeReverse:
		while (true) {
			if (n <= 0) {
				return kept;
			} else {
				if (!list.b) {
					return kept;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs,
						$temp$kept = A2($elm$core$List$cons, x, kept);
					n = $temp$n;
					list = $temp$list;
					kept = $temp$kept;
					continue takeReverse;
				}
			}
		}
	});
var $elm$core$List$takeTailRec = F2(
	function (n, list) {
		return $elm$core$List$reverse(
			A3($elm$core$List$takeReverse, n, list, _List_Nil));
	});
var $elm$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (n <= 0) {
			return _List_Nil;
		} else {
			var _v0 = _Utils_Tuple2(n, list);
			_v0$1:
			while (true) {
				_v0$5:
				while (true) {
					if (!_v0.b.b) {
						return list;
					} else {
						if (_v0.b.b.b) {
							switch (_v0.a) {
								case 1:
									break _v0$1;
								case 2:
									var _v2 = _v0.b;
									var x = _v2.a;
									var _v3 = _v2.b;
									var y = _v3.a;
									return _List_fromArray(
										[x, y]);
								case 3:
									if (_v0.b.b.b.b) {
										var _v4 = _v0.b;
										var x = _v4.a;
										var _v5 = _v4.b;
										var y = _v5.a;
										var _v6 = _v5.b;
										var z = _v6.a;
										return _List_fromArray(
											[x, y, z]);
									} else {
										break _v0$5;
									}
								default:
									if (_v0.b.b.b.b && _v0.b.b.b.b.b) {
										var _v7 = _v0.b;
										var x = _v7.a;
										var _v8 = _v7.b;
										var y = _v8.a;
										var _v9 = _v8.b;
										var z = _v9.a;
										var _v10 = _v9.b;
										var w = _v10.a;
										var tl = _v10.b;
										return (ctr > 1000) ? A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A2($elm$core$List$takeTailRec, n - 4, tl))))) : A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A3($elm$core$List$takeFast, ctr + 1, n - 4, tl)))));
									} else {
										break _v0$5;
									}
							}
						} else {
							if (_v0.a === 1) {
								break _v0$1;
							} else {
								break _v0$5;
							}
						}
					}
				}
				return list;
			}
			var _v1 = _v0.b;
			var x = _v1.a;
			return _List_fromArray(
				[x]);
		}
	});
var $elm$core$List$take = F2(
	function (n, list) {
		return A3($elm$core$List$takeFast, 0, n, list);
	});
var $author$project$History$push = F2(
	function (a, _v0) {
		var guts = _v0.a;
		return $author$project$History$History(
			_Utils_update(
				guts,
				{
					current: a,
					future: _List_Nil,
					past: A2(
						$elm$core$List$take,
						guts.retention,
						A2($elm$core$List$cons, guts.current, guts.past))
				}));
	});
var $author$project$History$mapPush = F2(
	function (fn, history) {
		return A2(
			$author$project$History$push,
			fn(
				$author$project$History$current(history)),
			history);
	});
var $author$project$Main$maybeAutoSave = function (_v0) {
	var model = _v0.a;
	var cmd = _v0.b;
	return model.autoSave ? _Utils_Tuple2(model, cmd) : _Utils_Tuple2(model, cmd);
};
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $elm$http$Http$expectBytesResponse = F2(
	function (toMsg, toResult) {
		return A3(
			_Http_expect,
			'arraybuffer',
			_Http_toDataView,
			A2($elm$core$Basics$composeR, toResult, toMsg));
	});
var $elm$http$Http$expectWhatever = function (toMsg) {
	return A2(
		$elm$http$Http$expectBytesResponse,
		toMsg,
		$elm$http$Http$resolve(
			function (_v0) {
				return $elm$core$Result$Ok(_Utils_Tuple0);
			}));
};
var $author$project$Supabase$voteEdgeFunction = F5(
	function (config, aId, bId, winnerId, toMsg) {
		return $elm$http$Http$request(
			{
				body: $elm$http$Http$jsonBody(
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'a_id',
								$elm$json$Json$Encode$int(aId)),
								_Utils_Tuple2(
								'b_id',
								$elm$json$Json$Encode$int(bId)),
								_Utils_Tuple2(
								'winner',
								$elm$json$Json$Encode$int(winnerId))
							]))),
				expect: $elm$http$Http$expectWhatever(toMsg),
				headers: _List_fromArray(
					[
						A2($elm$http$Http$header, 'apikey', config.anonKey),
						A2($elm$http$Http$header, 'Authorization', 'Bearer ' + config.anonKey),
						A2($elm$http$Http$header, 'Content-Type', 'application/json')
					]),
				method: 'POST',
				timeout: $elm$core$Maybe$Nothing,
				tracker: $elm$core$Maybe$Nothing,
				url: config.url + '/functions/v1/vote'
			});
	});
var $author$project$Main$handleMatchFinished = F2(
	function (outcome, model) {
		if ($author$project$Main$isVotingDisabled(model)) {
			return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
		} else {
			var currentLeague = $author$project$History$current(model.history);
			var _v0 = $author$project$League$currentMatch(currentLeague);
			if (_v0.$ === 'Just') {
				var _v1 = _v0.a;
				var playerA = _v1.a;
				var playerB = _v1.b;
				var updatedHistory = A2(
					$author$project$History$mapPush,
					$author$project$League$finishMatch(outcome),
					model.history);
				var updatedModel = _Utils_update(
					model,
					{history: updatedHistory});
				var playerBId = function () {
					var _v5 = $author$project$Player$id(playerB);
					var id = _v5.a;
					return id;
				}();
				var playerAId = function () {
					var _v4 = $author$project$Player$id(playerA);
					var id = _v4.a;
					return id;
				}();
				var winnerId = function () {
					if (outcome.$ === 'Win') {
						var won = outcome.a.won;
						var _v3 = $author$project$Player$id(won);
						var id = _v3.a;
						return id;
					} else {
						return playerAId;
					}
				}();
				var matchCmd = A5($author$project$Supabase$voteEdgeFunction, $author$project$Config$supabaseConfig, playerAId, playerBId, winnerId, $author$project$Main$MatchSaved);
				return $author$project$Main$maybeAutoSave(
					$author$project$Main$startNextMatchIfPossible(
						_Utils_Tuple2(updatedModel, matchCmd)));
			} else {
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			}
		}
	});
var $author$project$Main$httpErrorToString = function (err) {
	switch (err.$) {
		case 'BadUrl':
			var u = err.a;
			return 'Bad URL: ' + u;
		case 'Timeout':
			return 'Request timed out';
		case 'NetworkError':
			return 'Network error';
		case 'BadStatus':
			var s = err.a;
			return 'Bad status: ' + $elm$core$String$fromInt(s);
		default:
			var b = err.a;
			return 'Bad body: ' + b;
	}
};
var $author$project$League$isPlayerIgnored = F2(
	function (player, _v0) {
		var league = _v0.a;
		return A2(
			$elm$core$List$member,
			$author$project$Player$id(player),
			league.ignored);
	});
var $author$project$History$mapInPlace = F2(
	function (fn, _v0) {
		var guts = _v0.a;
		return $author$project$History$History(
			_Utils_update(
				guts,
				{
					current: fn(guts.current)
				}));
	});
var $author$project$Player$name = function (_v0) {
	var player = _v0.a;
	return player.name;
};
var $author$project$Main$AMOnly = {$: 'AMOnly'};
var $author$project$Main$PMOnly = {$: 'PMOnly'};
var $elm$core$String$toLower = _String_toLower;
var $author$project$Main$parseFilter = function (s) {
	var _v0 = $elm$core$String$toLower(s);
	switch (_v0) {
		case 'all':
			return $elm$core$Maybe$Just($author$project$Main$All);
		case 'am':
			return $elm$core$Maybe$Just($author$project$Main$AMOnly);
		case 'pm':
			return $elm$core$Maybe$Just($author$project$Main$PMOnly);
		default:
			return $elm$core$Maybe$Nothing;
	}
};
var $author$project$League$players = function (_v0) {
	var league = _v0.a;
	return $rtfeldman$elm_sorter_experiment$Sort$Dict$values(league.players);
};
var $elm$core$Set$remove = F2(
	function (key, _v0) {
		var dict = _v0.a;
		return $elm$core$Set$Set_elm_builtin(
			A2($elm$core$Dict$remove, key, dict));
	});
var $rtfeldman$elm_sorter_experiment$Sort$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === 'Node') && (dict.e.$ === 'Node')) {
			var left = dict.e;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var $rtfeldman$elm_sorter_experiment$Sort$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === 'Node') && (dict.e.$ === 'Node')) && (dict.f.$ === 'Node')) {
		if ((dict.f.e.$ === 'Node') && (dict.f.e.b.$ === 'Red')) {
			var sorter = dict.a;
			var clr = dict.b;
			var k = dict.c;
			var v = dict.d;
			var _v1 = dict.e;
			var lClr = _v1.b;
			var lK = _v1.c;
			var lV = _v1.d;
			var lLeft = _v1.e;
			var lRight = _v1.f;
			var _v2 = dict.f;
			var rClr = _v2.b;
			var rK = _v2.c;
			var rV = _v2.d;
			var rLeft = _v2.e;
			var _v3 = rLeft.b;
			var rlK = rLeft.c;
			var rlV = rLeft.d;
			var rlL = rLeft.e;
			var rlR = rLeft.f;
			var rRight = _v2.f;
			return A6(
				$rtfeldman$elm_sorter_experiment$Internal$Dict$Node,
				sorter,
				$rtfeldman$elm_sorter_experiment$Internal$Dict$Red,
				rlK,
				rlV,
				A6(
					$rtfeldman$elm_sorter_experiment$Internal$Dict$Node,
					sorter,
					$rtfeldman$elm_sorter_experiment$Internal$Dict$Black,
					k,
					v,
					A6($rtfeldman$elm_sorter_experiment$Internal$Dict$Node, sorter, $rtfeldman$elm_sorter_experiment$Internal$Dict$Red, lK, lV, lLeft, lRight),
					rlL),
				A6($rtfeldman$elm_sorter_experiment$Internal$Dict$Node, sorter, $rtfeldman$elm_sorter_experiment$Internal$Dict$Black, rK, rV, rlR, rRight));
		} else {
			var sorter = dict.a;
			var clr = dict.b;
			var k = dict.c;
			var v = dict.d;
			var _v4 = dict.e;
			var lClr = _v4.b;
			var lK = _v4.c;
			var lV = _v4.d;
			var lLeft = _v4.e;
			var lRight = _v4.f;
			var _v5 = dict.f;
			var rClr = _v5.b;
			var rK = _v5.c;
			var rV = _v5.d;
			var rLeft = _v5.e;
			var rRight = _v5.f;
			if (clr.$ === 'Black') {
				return A6(
					$rtfeldman$elm_sorter_experiment$Internal$Dict$Node,
					sorter,
					$rtfeldman$elm_sorter_experiment$Internal$Dict$Black,
					k,
					v,
					A6($rtfeldman$elm_sorter_experiment$Internal$Dict$Node, sorter, $rtfeldman$elm_sorter_experiment$Internal$Dict$Red, lK, lV, lLeft, lRight),
					A6($rtfeldman$elm_sorter_experiment$Internal$Dict$Node, sorter, $rtfeldman$elm_sorter_experiment$Internal$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A6(
					$rtfeldman$elm_sorter_experiment$Internal$Dict$Node,
					sorter,
					$rtfeldman$elm_sorter_experiment$Internal$Dict$Black,
					k,
					v,
					A6($rtfeldman$elm_sorter_experiment$Internal$Dict$Node, sorter, $rtfeldman$elm_sorter_experiment$Internal$Dict$Red, lK, lV, lLeft, lRight),
					A6($rtfeldman$elm_sorter_experiment$Internal$Dict$Node, sorter, $rtfeldman$elm_sorter_experiment$Internal$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $rtfeldman$elm_sorter_experiment$Sort$Dict$moveRedRight = function (dict) {
	if (((dict.$ === 'Node') && (dict.e.$ === 'Node')) && (dict.f.$ === 'Node')) {
		if ((dict.e.e.$ === 'Node') && (dict.e.e.b.$ === 'Red')) {
			var sorter = dict.a;
			var clr = dict.b;
			var k = dict.c;
			var v = dict.d;
			var _v1 = dict.e;
			var lClr = _v1.b;
			var lK = _v1.c;
			var lV = _v1.d;
			var _v2 = _v1.e;
			var _v3 = _v2.b;
			var llK = _v2.c;
			var llV = _v2.d;
			var llLeft = _v2.e;
			var llRight = _v2.f;
			var lRight = _v1.f;
			var _v4 = dict.f;
			var rClr = _v4.b;
			var rK = _v4.c;
			var rV = _v4.d;
			var rLeft = _v4.e;
			var rRight = _v4.f;
			return A6(
				$rtfeldman$elm_sorter_experiment$Internal$Dict$Node,
				sorter,
				$rtfeldman$elm_sorter_experiment$Internal$Dict$Red,
				lK,
				lV,
				A6($rtfeldman$elm_sorter_experiment$Internal$Dict$Node, sorter, $rtfeldman$elm_sorter_experiment$Internal$Dict$Black, llK, llV, llLeft, llRight),
				A6(
					$rtfeldman$elm_sorter_experiment$Internal$Dict$Node,
					sorter,
					$rtfeldman$elm_sorter_experiment$Internal$Dict$Black,
					k,
					v,
					lRight,
					A6($rtfeldman$elm_sorter_experiment$Internal$Dict$Node, sorter, $rtfeldman$elm_sorter_experiment$Internal$Dict$Red, rK, rV, rLeft, rRight)));
		} else {
			var sorter = dict.a;
			var clr = dict.b;
			var k = dict.c;
			var v = dict.d;
			var _v5 = dict.e;
			var lClr = _v5.b;
			var lK = _v5.c;
			var lV = _v5.d;
			var lLeft = _v5.e;
			var lRight = _v5.f;
			var _v6 = dict.f;
			var rClr = _v6.b;
			var rK = _v6.c;
			var rV = _v6.d;
			var rLeft = _v6.e;
			var rRight = _v6.f;
			if (clr.$ === 'Black') {
				return A6(
					$rtfeldman$elm_sorter_experiment$Internal$Dict$Node,
					sorter,
					$rtfeldman$elm_sorter_experiment$Internal$Dict$Black,
					k,
					v,
					A6($rtfeldman$elm_sorter_experiment$Internal$Dict$Node, sorter, $rtfeldman$elm_sorter_experiment$Internal$Dict$Red, lK, lV, lLeft, lRight),
					A6($rtfeldman$elm_sorter_experiment$Internal$Dict$Node, sorter, $rtfeldman$elm_sorter_experiment$Internal$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A6(
					$rtfeldman$elm_sorter_experiment$Internal$Dict$Node,
					sorter,
					$rtfeldman$elm_sorter_experiment$Internal$Dict$Black,
					k,
					v,
					A6($rtfeldman$elm_sorter_experiment$Internal$Dict$Node, sorter, $rtfeldman$elm_sorter_experiment$Internal$Dict$Red, lK, lV, lLeft, lRight),
					A6($rtfeldman$elm_sorter_experiment$Internal$Dict$Node, sorter, $rtfeldman$elm_sorter_experiment$Internal$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $rtfeldman$elm_sorter_experiment$Sort$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === 'Node') && (left.b.$ === 'Red')) {
			var sorter = left.a;
			var _v1 = left.b;
			var lK = left.c;
			var lV = left.d;
			var lLeft = left.e;
			var lRight = left.f;
			return A6(
				$rtfeldman$elm_sorter_experiment$Internal$Dict$Node,
				sorter,
				color,
				lK,
				lV,
				lLeft,
				A6($rtfeldman$elm_sorter_experiment$Internal$Dict$Node, sorter, $rtfeldman$elm_sorter_experiment$Internal$Dict$Red, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === 'Node') && (right.b.$ === 'Black')) {
					if (right.e.$ === 'Node') {
						if (right.e.b.$ === 'Black') {
							var _v3 = right.b;
							var _v4 = right.e;
							var _v5 = _v4.b;
							return $rtfeldman$elm_sorter_experiment$Sort$Dict$moveRedRight(dict);
						} else {
							break _v2$2;
						}
					} else {
						var _v6 = right.b;
						return $rtfeldman$elm_sorter_experiment$Sort$Dict$moveRedRight(dict);
					}
				} else {
					break _v2$2;
				}
			}
			return dict;
		}
	});
var $rtfeldman$elm_sorter_experiment$Sort$Dict$removeMin = function (dict) {
	if (dict.$ === 'Node') {
		if (dict.e.$ === 'Node') {
			var sorter = dict.a;
			var color = dict.b;
			var key = dict.c;
			var value = dict.d;
			var left = dict.e;
			var lColor = left.b;
			var lLeft = left.e;
			var right = dict.f;
			if (lColor.$ === 'Black') {
				if ((lLeft.$ === 'Node') && (lLeft.b.$ === 'Red')) {
					var _v3 = lLeft.b;
					return A6(
						$rtfeldman$elm_sorter_experiment$Internal$Dict$Node,
						sorter,
						color,
						key,
						value,
						$rtfeldman$elm_sorter_experiment$Sort$Dict$removeMin(left),
						right);
				} else {
					var _v4 = $rtfeldman$elm_sorter_experiment$Sort$Dict$moveRedLeft(dict);
					if (_v4.$ === 'Node') {
						var movedColor = _v4.b;
						var movedKey = _v4.c;
						var movedValue = _v4.d;
						var movedLeft = _v4.e;
						var movedRight = _v4.f;
						return A6(
							$rtfeldman$elm_sorter_experiment$Sort$Dict$balance,
							sorter,
							movedColor,
							movedKey,
							movedValue,
							$rtfeldman$elm_sorter_experiment$Sort$Dict$removeMin(movedLeft),
							movedRight);
					} else {
						var leaf = _v4;
						return leaf;
					}
				}
			} else {
				return A6(
					$rtfeldman$elm_sorter_experiment$Internal$Dict$Node,
					sorter,
					color,
					key,
					value,
					$rtfeldman$elm_sorter_experiment$Sort$Dict$removeMin(left),
					right);
			}
		} else {
			var leaf = dict.e;
			return leaf;
		}
	} else {
		var leaf = dict;
		return leaf;
	}
};
var $rtfeldman$elm_sorter_experiment$Sort$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === 'Leaf') {
			var leaf = dict;
			return leaf;
		} else {
			var sorter = dict.a;
			var color = dict.b;
			var key = dict.c;
			var value = dict.d;
			var left = dict.e;
			var right = dict.f;
			var _v3 = A3($rtfeldman$elm_sorter_experiment$Sort$toOrder, sorter, targetKey, key);
			if (_v3.$ === 'LT') {
				if ((left.$ === 'Node') && (left.b.$ === 'Black')) {
					var _v5 = left.b;
					var lLeft = left.e;
					if ((lLeft.$ === 'Node') && (lLeft.b.$ === 'Red')) {
						var _v7 = lLeft.b;
						return A6(
							$rtfeldman$elm_sorter_experiment$Internal$Dict$Node,
							sorter,
							color,
							key,
							value,
							A2($rtfeldman$elm_sorter_experiment$Sort$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _v8 = $rtfeldman$elm_sorter_experiment$Sort$Dict$moveRedLeft(dict);
						if (_v8.$ === 'Node') {
							var movedColor = _v8.b;
							var movedKey = _v8.c;
							var movedValue = _v8.d;
							var movedLeft = _v8.e;
							var movedRight = _v8.f;
							return A6(
								$rtfeldman$elm_sorter_experiment$Sort$Dict$balance,
								sorter,
								movedColor,
								movedKey,
								movedValue,
								A2($rtfeldman$elm_sorter_experiment$Sort$Dict$removeHelp, targetKey, movedLeft),
								movedRight);
						} else {
							var leaf = _v8;
							return leaf;
						}
					}
				} else {
					return A6(
						$rtfeldman$elm_sorter_experiment$Internal$Dict$Node,
						sorter,
						color,
						key,
						value,
						A2($rtfeldman$elm_sorter_experiment$Sort$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					$rtfeldman$elm_sorter_experiment$Sort$Dict$removeHelpEQGT,
					targetKey,
					A7($rtfeldman$elm_sorter_experiment$Sort$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var $rtfeldman$elm_sorter_experiment$Sort$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === 'Node') {
			var sorter = dict.a;
			var color = dict.b;
			var key = dict.c;
			var value = dict.d;
			var left = dict.e;
			var right = dict.f;
			if (_Utils_eq(targetKey, key)) {
				var _v1 = $rtfeldman$elm_sorter_experiment$Sort$Dict$getMin(right);
				if (_v1.$ === 'Node') {
					var minKey = _v1.c;
					var minValue = _v1.d;
					return A6(
						$rtfeldman$elm_sorter_experiment$Sort$Dict$balance,
						sorter,
						color,
						minKey,
						minValue,
						left,
						$rtfeldman$elm_sorter_experiment$Sort$Dict$removeMin(right));
				} else {
					var leaf = _v1;
					return leaf;
				}
			} else {
				return A6(
					$rtfeldman$elm_sorter_experiment$Sort$Dict$balance,
					sorter,
					color,
					key,
					value,
					left,
					A2($rtfeldman$elm_sorter_experiment$Sort$Dict$removeHelp, targetKey, right));
			}
		} else {
			var leaf = dict;
			return leaf;
		}
	});
var $rtfeldman$elm_sorter_experiment$Sort$Dict$remove = F2(
	function (targetKey, dict) {
		var _v0 = A2($rtfeldman$elm_sorter_experiment$Sort$Dict$removeHelp, targetKey, dict);
		if ((_v0.$ === 'Node') && (_v0.b.$ === 'Red')) {
			var sorter = _v0.a;
			var _v1 = _v0.b;
			var k = _v0.c;
			var v = _v0.d;
			var l = _v0.e;
			var r = _v0.f;
			return A6($rtfeldman$elm_sorter_experiment$Internal$Dict$Node, sorter, $rtfeldman$elm_sorter_experiment$Internal$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $author$project$League$retirePlayer = F2(
	function (player, _v0) {
		var league = _v0.a;
		return $author$project$League$League(
			_Utils_update(
				league,
				{
					currentMatch: function () {
						var _v1 = league.currentMatch;
						if (_v1.$ === 'Nothing') {
							return $elm$core$Maybe$Nothing;
						} else {
							var _v2 = _v1.a;
							var a = _v2.a;
							var b = _v2.b;
							return (_Utils_eq(
								$author$project$Player$id(player),
								$author$project$Player$id(a)) || _Utils_eq(
								$author$project$Player$id(player),
								$author$project$Player$id(b))) ? $elm$core$Maybe$Nothing : league.currentMatch;
						}
					}(),
					ignored: A2(
						$elm$core$List$filter,
						$elm$core$Basics$neq(
							$author$project$Player$id(player)),
						league.ignored),
					players: A2(
						$rtfeldman$elm_sorter_experiment$Sort$Dict$remove,
						$author$project$Player$id(player),
						league.players)
				}));
	});
var $author$project$Main$saveIgnoredPlayers = _Platform_outgoingPort('saveIgnoredPlayers', $elm$json$Json$Encode$string);
var $author$project$Player$setAM = F2(
	function (val, _v0) {
		var player = _v0.a;
		return $author$project$Player$Player(
			_Utils_update(
				player,
				{am: val}));
	});
var $author$project$Player$setPM = F2(
	function (val, _v0) {
		var player = _v0.a;
		return $author$project$Player$Player(
			_Utils_update(
				player,
				{pm: val}));
	});
var $author$project$Main$ClearStatus = {$: 'ClearStatus'};
var $elm$core$Process$sleep = _Process_sleep;
var $author$project$Main$setTemporaryStatus = F2(
	function (message, model) {
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{
					isStatusTemporary: true,
					status: $elm$core$Maybe$Just(message)
				}),
			A2(
				$elm$core$Task$perform,
				function (_v0) {
					return $author$project$Main$ClearStatus;
				},
				$elm$core$Process$sleep(2000)));
	});
var $elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (maybeValue.$ === 'Just') {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$core$Maybe$map2 = F3(
	function (func, ma, mb) {
		if (ma.$ === 'Nothing') {
			return $elm$core$Maybe$Nothing;
		} else {
			var a = ma.a;
			if (mb.$ === 'Nothing') {
				return $elm$core$Maybe$Nothing;
			} else {
				var b = mb.a;
				return $elm$core$Maybe$Just(
					A2(func, a, b));
			}
		}
	});
var $author$project$League$startMatch = F2(
	function (_v0, _v1) {
		var playerA = _v0.a;
		var playerB = _v0.b;
		var league = _v1.a;
		return $author$project$League$League(
			_Utils_update(
				league,
				{
					currentMatch: A2(
						$elm$core$Maybe$andThen,
						function (_v2) {
							var gotA = _v2.a;
							var gotB = _v2.b;
							return (!_Utils_eq(gotA, gotB)) ? $elm$core$Maybe$Just(
								A2($author$project$League$Match, gotA, gotB)) : $elm$core$Maybe$Nothing;
						},
						A3(
							$elm$core$Maybe$map2,
							$elm$core$Tuple$pair,
							A2(
								$rtfeldman$elm_sorter_experiment$Sort$Dict$get,
								$author$project$Player$id(playerA),
								league.players),
							A2(
								$rtfeldman$elm_sorter_experiment$Sort$Dict$get,
								$author$project$Player$id(playerB),
								league.players)))
				}));
	});
var $author$project$Player$PlayerId = function (a) {
	return {$: 'PlayerId', a: a};
};
var $author$project$Main$supabasePlayerToPlayer = function (supabasePlayer) {
	return $author$project$Player$Player(
		{
			am: supabasePlayer.playsAM,
			id: $author$project$Player$PlayerId(supabasePlayer.id),
			matches: supabasePlayer.matchesPlayed,
			name: supabasePlayer.name,
			pm: supabasePlayer.playsPM,
			rating: supabasePlayer.rating
		});
};
var $author$project$Main$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'TogglePlayerAM':
				var player = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							timeToggleConfirmation: $elm$core$Maybe$Just(
								_Utils_Tuple2(player, 'AM'))
						}),
					$elm$core$Platform$Cmd$none);
			case 'TogglePlayerPM':
				var player = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							timeToggleConfirmation: $elm$core$Maybe$Just(
								_Utils_Tuple2(player, 'PM'))
						}),
					$elm$core$Platform$Cmd$none);
			case 'ConfirmTogglePlayerTime':
				var player = msg.a;
				var timeType = msg.b;
				var newState = msg.c;
				var updatedLeague = function (league) {
					var _v2 = A2(
						$author$project$League$getPlayer,
						$author$project$Player$id(player),
						league);
					if (_v2.$ === 'Just') {
						var p = _v2.a;
						var newP = (timeType === 'AM') ? A2($author$project$Player$setAM, newState, p) : A2($author$project$Player$setPM, newState, p);
						return A2($author$project$League$updatePlayer, newP, league);
					} else {
						return league;
					}
				}(
					$author$project$History$current(model.history));
				return $author$project$Main$maybeAutoSave(
					$author$project$Main$startNextMatchIfPossible(
						_Utils_Tuple2(
							_Utils_update(
								model,
								{
									history: A2(
										$author$project$History$mapPush,
										function (_v1) {
											return updatedLeague;
										},
										model.history),
									timeToggleConfirmation: $elm$core$Maybe$Nothing
								}),
							$elm$core$Platform$Cmd$none)));
			case 'CancelTogglePlayerTime':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{timeToggleConfirmation: $elm$core$Maybe$Nothing}),
					$elm$core$Platform$Cmd$none);
			case 'KeeperUpdatedNewPlayerName':
				var newPlayerName = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{newPlayerName: newPlayerName}),
					$elm$core$Platform$Cmd$none);
			case 'KeeperWantsToAddNewPlayer':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{showAddPlayerPopup: true}),
					$elm$core$Platform$Cmd$none);
			case 'KeeperWantsToShowAddPlayerPopup':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{showAddPlayerPopup: true}),
					$elm$core$Platform$Cmd$none);
			case 'KeeperWantsToHideAddPlayerPopup':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{addPlayerAM: true, addPlayerName: '', addPlayerPM: true, addPlayerRating: '500', showAddPlayerPopup: false}),
					$elm$core$Platform$Cmd$none);
			case 'KeeperUpdatedAddPlayerName':
				var name = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{addPlayerName: name}),
					$elm$core$Platform$Cmd$none);
			case 'KeeperUpdatedAddPlayerRating':
				var rating = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{addPlayerRating: rating}),
					$elm$core$Platform$Cmd$none);
			case 'KeeperToggledAddPlayerAM':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{addPlayerAM: !model.addPlayerAM}),
					$elm$core$Platform$Cmd$none);
			case 'KeeperToggledAddPlayerPM':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{addPlayerPM: !model.addPlayerPM}),
					$elm$core$Platform$Cmd$none);
			case 'KeeperConfirmedAddPlayer':
				var rating = A2(
					$elm$core$Maybe$withDefault,
					500,
					$elm$core$String$toInt(model.addPlayerRating));
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{addPlayerAM: true, addPlayerName: '', addPlayerPM: true, addPlayerRating: '500', showAddPlayerPopup: false}),
					A6($author$project$Supabase$createNewPlayer, $author$project$Config$supabaseConfig, model.addPlayerName, rating, model.addPlayerAM, model.addPlayerPM, $author$project$Main$NewPlayerCreated));
			case 'KeeperWantsToRetirePlayer':
				var player = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							playerDeletionConfirmation: $elm$core$Maybe$Just(
								_Utils_Tuple2(player, 1))
						}),
					$elm$core$Platform$Cmd$none);
			case 'ConfirmPlayerDeletion':
				var player = msg.a;
				var step = msg.b;
				if (step === 2) {
					var _v3 = $author$project$Player$id(player);
					var playerId = _v3.a;
					var newIgnoredPlayers = A2(
						$elm$core$Set$insert,
						$elm$core$String$fromInt(playerId),
						model.ignoredPlayers);
					var serializedIgnored = A2(
						$elm$core$String$join,
						',',
						$elm$core$Set$toList(newIgnoredPlayers));
					return $author$project$Main$maybeAutoSave(
						$author$project$Main$startNextMatchIfPossible(
							_Utils_Tuple2(
								_Utils_update(
									model,
									{
										history: A2(
											$author$project$History$mapPush,
											$author$project$League$retirePlayer(player),
											model.history),
										ignoredPlayers: newIgnoredPlayers,
										isStatusTemporary: false,
										playerDeletionConfirmation: $elm$core$Maybe$Nothing,
										status: $elm$core$Maybe$Just('Attempting to delete player...')
									}),
								$elm$core$Platform$Cmd$batch(
									_List_fromArray(
										[
											A3($author$project$Supabase$deletePlayer, $author$project$Config$supabaseConfig, playerId, $author$project$Main$PlayerDeleted),
											$author$project$Main$saveIgnoredPlayers(serializedIgnored)
										])))));
				} else {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								playerDeletionConfirmation: $elm$core$Maybe$Just(
									_Utils_Tuple2(player, 2))
							}),
						$elm$core$Platform$Cmd$none);
				}
			case 'CancelPlayerDeletion':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{playerDeletionConfirmation: $elm$core$Maybe$Nothing}),
					$elm$core$Platform$Cmd$none);
			case 'KeeperWantsToIgnorePlayer':
				var player = msg.a;
				var _v4 = $author$project$Player$id(player);
				var idInt = _v4.a;
				var newIgnoredPlayers = A2(
					$elm$core$Set$insert,
					$elm$core$String$fromInt(idInt),
					model.ignoredPlayers);
				var serializedIgnored = A2(
					$elm$core$String$join,
					',',
					$elm$core$Set$toList(newIgnoredPlayers));
				return $author$project$Main$startNextMatchIfPossible(
					_Utils_Tuple2(
						_Utils_update(
							model,
							{ignoredPlayers: newIgnoredPlayers}),
						$author$project$Main$saveIgnoredPlayers(serializedIgnored)));
			case 'KeeperWantsToUnignorePlayer':
				var player = msg.a;
				var _v5 = $author$project$Player$id(player);
				var idInt = _v5.a;
				var newIgnoredPlayers = A2(
					$elm$core$Set$remove,
					$elm$core$String$fromInt(idInt),
					model.ignoredPlayers);
				var serializedIgnored = A2(
					$elm$core$String$join,
					',',
					$elm$core$Set$toList(newIgnoredPlayers));
				return $author$project$Main$startNextMatchIfPossible(
					_Utils_Tuple2(
						_Utils_update(
							model,
							{ignoredPlayers: newIgnoredPlayers}),
						$author$project$Main$saveIgnoredPlayers(serializedIgnored)));
			case 'KeeperWantsToSkipMatch':
				return $author$project$Main$startNextMatchIfPossible(
					_Utils_Tuple2(
						_Utils_update(
							model,
							{
								history: A2($author$project$History$mapPush, $author$project$League$clearMatch, model.history)
							}),
						$elm$core$Platform$Cmd$none));
			case 'ClearStatus':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{isStatusTemporary: false, status: $elm$core$Maybe$Nothing}),
					$elm$core$Platform$Cmd$none);
			case 'GotNextMatch':
				if (msg.a.$ === 'Just') {
					var match = msg.a.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								history: A2(
									$author$project$History$mapInPlace,
									$author$project$League$startMatch(match),
									model.history)
							}),
						$elm$core$Platform$Cmd$none);
				} else {
					var _v6 = msg.a;
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 'MatchFinished':
				var outcome = msg.a;
				return A2($author$project$Main$handleMatchFinished, outcome, model);
			case 'MatchSaved':
				var result = msg.a;
				if (result.$ === 'Ok') {
					var newVoteCount = model.votesSinceLastSync + 1;
					var shouldSync = newVoteCount >= 25;
					return shouldSync ? _Utils_Tuple2(
						_Utils_update(
							model,
							{
								isStatusTemporary: false,
								status: $elm$core$Maybe$Just('Syncing data...'),
								votesSinceLastSync: 0
							}),
						A2(
							$elm$core$Task$perform,
							function (_v8) {
								return $author$project$Main$TriggerReload;
							},
							$elm$core$Process$sleep(200))) : _Utils_Tuple2(
						_Utils_update(
							model,
							{status: $elm$core$Maybe$Nothing, votesSinceLastSync: newVoteCount}),
						$elm$core$Platform$Cmd$none);
				} else {
					var err = result.a;
					var debugInfo = function () {
						var _v9 = model.status;
						if (_v9.$ === 'Just') {
							var s = _v9.a;
							return s;
						} else {
							return '';
						}
					}();
					var errorMsg = 'Failed to save match: ' + ($author$project$Main$httpErrorToString(err) + ('\n' + (debugInfo + ' (Try voting again)')));
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								isStatusTemporary: false,
								status: $elm$core$Maybe$Just(errorMsg)
							}),
						$elm$core$Platform$Cmd$none);
				}
			case 'LeagueStateSaved':
				var result = msg.a;
				if (result.$ === 'Ok') {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				} else {
					var err = result.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								isStatusTemporary: false,
								status: $elm$core$Maybe$Just(
									'Failed to update league state: ' + $author$project$Main$httpErrorToString(err))
							}),
						$elm$core$Platform$Cmd$none);
				}
			case 'PlayerACreated':
				var result = msg.b;
				if (result.$ === 'Ok') {
					var supabasePlayer = result.a;
					var player = $author$project$Main$supabasePlayerToPlayer(supabasePlayer);
					return $author$project$Main$maybeAutoSave(
						$author$project$Main$startNextMatchIfPossible(
							_Utils_Tuple2(
								_Utils_update(
									model,
									{
										history: A2(
											$author$project$History$mapPush,
											$author$project$League$addPlayer(player),
											model.history)
									}),
								$elm$core$Platform$Cmd$none)));
				} else {
					var err = result.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								isStatusTemporary: false,
								status: $elm$core$Maybe$Just(
									'Failed to create player: ' + $author$project$Main$httpErrorToString(err))
							}),
						$elm$core$Platform$Cmd$none);
				}
			case 'PlayerBCreated':
				var result = msg.b;
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			case 'NewPlayerCreated':
				var result = msg.a;
				if (result.$ === 'Ok') {
					var supabasePlayer = result.a;
					var player = $author$project$Main$supabasePlayerToPlayer(supabasePlayer);
					return $author$project$Main$maybeAutoSave(
						$author$project$Main$startNextMatchIfPossible(
							_Utils_Tuple2(
								_Utils_update(
									model,
									{
										history: A2(
											$author$project$History$mapPush,
											$author$project$League$addPlayer(player),
											model.history)
									}),
								$elm$core$Platform$Cmd$none)));
				} else {
					var err = result.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								isStatusTemporary: false,
								status: $elm$core$Maybe$Just(
									'Failed to create player: ' + $author$project$Main$httpErrorToString(err))
							}),
						$elm$core$Platform$Cmd$none);
				}
			case 'PlayerDeleted':
				var result = msg.a;
				if (result.$ === 'Ok') {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								isStatusTemporary: true,
								status: $elm$core$Maybe$Just('Player successfully deleted from database')
							}),
						A2(
							$elm$core$Task$perform,
							function (_v14) {
								return $author$project$Main$TriggerReload;
							},
							$elm$core$Process$sleep(500)));
				} else {
					var err = result.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								isStatusTemporary: false,
								status: $elm$core$Maybe$Just(
									'Failed to delete player from database: ' + $author$project$Main$httpErrorToString(err))
							}),
						$elm$core$Platform$Cmd$none);
				}
			case 'PeriodicSync':
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			case 'TriggerReload':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{isSyncing: true, votesSinceLastSync: 0}),
					A2($author$project$Supabase$getPlayers, $author$project$Config$supabaseConfig, $author$project$Main$GotPlayers));
			case 'KeeperWantsToUndo':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							history: A2(
								$elm$core$Maybe$withDefault,
								model.history,
								$author$project$History$goBack(model.history))
						}),
					$elm$core$Platform$Cmd$none);
			case 'KeeperWantsToRedo':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							history: A2(
								$elm$core$Maybe$withDefault,
								model.history,
								$author$project$History$goForward(model.history))
						}),
					$elm$core$Platform$Cmd$none);
			case 'KeeperWantsToShowCustomMatchup':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{customMatchupPlayerA: $elm$core$Maybe$Nothing, customMatchupPlayerB: $elm$core$Maybe$Nothing, playerASearch: '', playerASearchResults: _List_Nil, playerBSearch: '', playerBSearchResults: _List_Nil, showCustomMatchup: true}),
					$elm$core$Platform$Cmd$none);
			case 'KeeperWantsToHideCustomMatchup':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{customMatchupPlayerA: $elm$core$Maybe$Nothing, customMatchupPlayerB: $elm$core$Maybe$Nothing, playerASearch: '', playerASearchResults: _List_Nil, playerBSearch: '', playerBSearchResults: _List_Nil, showCustomMatchup: false}),
					$elm$core$Platform$Cmd$none);
			case 'KeeperSelectedPlayerA':
				var player = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							customMatchupPlayerA: $elm$core$Maybe$Just(player),
							playerASearch: $author$project$Player$name(player),
							playerASearchResults: _List_Nil
						}),
					$elm$core$Platform$Cmd$none);
			case 'KeeperSelectedPlayerB':
				var player = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							customMatchupPlayerB: $elm$core$Maybe$Just(player),
							playerBSearch: $author$project$Player$name(player),
							playerBSearchResults: _List_Nil
						}),
					$elm$core$Platform$Cmd$none);
			case 'KeeperUpdatedPlayerASearch':
				var searchText = msg.a;
				var currentLeague = $author$project$History$current(model.history);
				var allPlayers = A2(
					$elm$core$List$sortBy,
					$author$project$Player$name,
					A2(
						$elm$core$List$filter,
						function (p) {
							return !A2($author$project$League$isPlayerIgnored, p, currentLeague);
						},
						$author$project$League$players(currentLeague)));
				var searchResults = ($elm$core$String$length(searchText) < 2) ? _List_Nil : A2(
					$elm$core$List$take,
					8,
					A2(
						$elm$core$List$filter,
						function (p) {
							return A2(
								$elm$core$String$contains,
								$elm$core$String$toLower(searchText),
								$elm$core$String$toLower(
									$author$project$Player$name(p)));
						},
						allPlayers));
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{playerASearch: searchText, playerASearchResults: searchResults}),
					$elm$core$Platform$Cmd$none);
			case 'KeeperUpdatedPlayerBSearch':
				var searchText = msg.a;
				var currentLeague = $author$project$History$current(model.history);
				var allPlayers = A2(
					$elm$core$List$sortBy,
					$author$project$Player$name,
					A2(
						$elm$core$List$filter,
						function (p) {
							return !A2($author$project$League$isPlayerIgnored, p, currentLeague);
						},
						$author$project$League$players(currentLeague)));
				var searchResults = ($elm$core$String$length(searchText) < 2) ? _List_Nil : A2(
					$elm$core$List$take,
					8,
					A2(
						$elm$core$List$filter,
						function (p) {
							return A2(
								$elm$core$String$contains,
								$elm$core$String$toLower(searchText),
								$elm$core$String$toLower(
									$author$project$Player$name(p)));
						},
						allPlayers));
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{playerBSearch: searchText, playerBSearchResults: searchResults}),
					$elm$core$Platform$Cmd$none);
			case 'KeeperWantsToStartCustomMatch':
				var _v15 = _Utils_Tuple2(model.customMatchupPlayerA, model.customMatchupPlayerB);
				if ((_v15.a.$ === 'Just') && (_v15.b.$ === 'Just')) {
					var playerA = _v15.a.a;
					var playerB = _v15.b.a;
					if (_Utils_eq(
						$author$project$Player$id(playerA),
						$author$project$Player$id(playerB))) {
						return A2($author$project$Main$setTemporaryStatus, 'Cannot match a player against themselves', model);
					} else {
						var updatedHistory = A2(
							$author$project$History$mapPush,
							A2(
								$elm$core$Basics$composeR,
								$author$project$League$clearMatch,
								$author$project$League$startMatch(
									A2($author$project$League$Match, playerA, playerB))),
							model.history);
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{customMatchupPlayerA: $elm$core$Maybe$Nothing, customMatchupPlayerB: $elm$core$Maybe$Nothing, history: updatedHistory, playerASearch: '', playerASearchResults: _List_Nil, playerBSearch: '', playerBSearchResults: _List_Nil, showCustomMatchup: false}),
							$elm$core$Platform$Cmd$none);
					}
				} else {
					return A2($author$project$Main$setTemporaryStatus, 'Please select both players for the custom match', model);
				}
			case 'LoadedLeague':
				if (msg.a.$ === 'Ok') {
					var league = msg.a.a;
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				} else {
					var problem = msg.a.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								isStatusTemporary: false,
								status: $elm$core$Maybe$Just('Failed to load standings: ' + problem)
							}),
						$elm$core$Platform$Cmd$none);
				}
			case 'GotPlayers':
				var result = msg.a;
				if (result.$ === 'Ok') {
					var supabasePlayers = result.a;
					var validIdPlayers = A2(
						$elm$core$List$filter,
						function (p) {
							return (p.id >= 1) && (p.id < 1000000);
						},
						supabasePlayers);
					var players = A2($elm$core$List$map, $author$project$Main$supabasePlayerToPlayer, validIdPlayers);
					var playerCount = $elm$core$List$length(players);
					var newLeague = A3($elm$core$List$foldl, $author$project$League$addPlayer, $author$project$League$init, players);
					var invalidSupabasePlayers = A2(
						$elm$core$List$filter,
						function (p) {
							return (p.id < 1) || (p.id >= 1000000);
						},
						supabasePlayers);
					var invalidCount = $elm$core$List$length(invalidSupabasePlayers);
					var statusMsg = (invalidCount > 0) ? ('Filtered out ' + ($elm$core$String$fromInt(invalidCount) + ' players with invalid IDs')) : '';
					var currentLeague = $author$project$History$current(model.history);
					var activeMatch = $author$project$League$currentMatch(currentLeague);
					var finalLeague = function () {
						if (activeMatch.$ === 'Just') {
							var match = activeMatch.a;
							return model.isSyncing ? A2($author$project$League$startMatch, match, newLeague) : newLeague;
						} else {
							return newLeague;
						}
					}();
					var updatedModel = _Utils_update(
						model,
						{
							history: A2($author$project$History$init, 50, finalLeague),
							isSyncing: false,
							votesSinceLastSync: 0
						});
					return (_Utils_eq(activeMatch, $elm$core$Maybe$Nothing) ? $author$project$Main$startNextMatchIfPossible : $elm$core$Basics$identity)(
						_Utils_Tuple2(
							updatedModel,
							$elm$core$String$isEmpty(statusMsg) ? $elm$core$Platform$Cmd$none : A2(
								$elm$core$Task$perform,
								$elm$core$Basics$identity,
								$elm$core$Task$succeed(
									$author$project$Main$ShowStatus(statusMsg)))));
				} else {
					var httpErr = result.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								isStatusTemporary: false,
								status: $elm$core$Maybe$Just(
									'Failed to fetch players from Supabase: ' + $author$project$Main$httpErrorToString(httpErr))
							}),
						$elm$core$Platform$Cmd$none);
				}
			case 'ReceivedCurrentTime':
				var time = msg.a;
				var timestamp = $elm$time$Time$posixToMillis(time);
				var updatedModel = _Utils_update(
					model,
					{lastModified: timestamp});
				return _Utils_Tuple2(updatedModel, $elm$core$Platform$Cmd$none);
			case 'ReceivedAutoSave':
				var value = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{autoSave: value}),
					$elm$core$Platform$Cmd$none);
			case 'ToggleAutoSave':
				var newVal = !model.autoSave;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{autoSave: newVal}),
					A2(
						$elm$core$Task$perform,
						$elm$core$Basics$identity,
						$elm$core$Task$succeed(
							$author$project$Main$ShowStatus(
								newVal ? 'Auto-save enabled' : 'Auto-save disabled'))));
			case 'ShowStatus':
				var message = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							isStatusTemporary: false,
							status: $elm$core$Maybe$Just(message)
						}),
					$elm$core$Platform$Cmd$none);
			case 'KeyPressed':
				var key = msg.a;
				var _v18 = _Utils_Tuple2(
					key,
					$author$project$League$currentMatch(
						$author$project$History$current(model.history)));
				_v18$5:
				while (true) {
					switch (_v18.a) {
						case '1':
							if (_v18.b.$ === 'Just') {
								var _v19 = _v18.b.a;
								var playerA = _v19.a;
								var playerB = _v19.b;
								if ($author$project$Main$isVotingDisabled(model)) {
									return A2($author$project$Main$setTemporaryStatus, 'Voting disabled during sync', model);
								} else {
									var outcome = $author$project$League$Win(
										{lost: playerB, won: playerA});
									return A2($author$project$Main$handleMatchFinished, outcome, model);
								}
							} else {
								break _v18$5;
							}
						case '2':
							if (_v18.b.$ === 'Just') {
								var _v20 = _v18.b.a;
								var playerA = _v20.a;
								var playerB = _v20.b;
								if ($author$project$Main$isVotingDisabled(model)) {
									return A2($author$project$Main$setTemporaryStatus, 'Voting disabled during sync', model);
								} else {
									var outcome = $author$project$League$Win(
										{lost: playerA, won: playerB});
									return A2($author$project$Main$handleMatchFinished, outcome, model);
								}
							} else {
								break _v18$5;
							}
						case '0':
							if (_v18.b.$ === 'Just') {
								var _v21 = _v18.b.a;
								var playerA = _v21.a;
								var playerB = _v21.b;
								if ($author$project$Main$isVotingDisabled(model)) {
									return A2($author$project$Main$setTemporaryStatus, 'Voting disabled during sync', model);
								} else {
									var outcome = $author$project$League$Draw(
										{playerA: playerA, playerB: playerB});
									return A2($author$project$Main$handleMatchFinished, outcome, model);
								}
							} else {
								break _v18$5;
							}
						case 'Escape':
							return $author$project$Main$startNextMatchIfPossible(
								_Utils_Tuple2(
									_Utils_update(
										model,
										{
											history: A2($author$project$History$mapInPlace, $author$project$League$clearMatch, model.history)
										}),
									$elm$core$Platform$Cmd$none));
						case 'Backspace':
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{
										history: A2(
											$elm$core$Maybe$withDefault,
											model.history,
											$author$project$History$goBack(model.history))
									}),
								$elm$core$Platform$Cmd$none);
						default:
							break _v18$5;
					}
				}
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			case 'IgnoredKey':
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			case 'SetTimeFilter':
				var tf = msg.a;
				return $author$project$Main$startNextMatchIfPossible(
					_Utils_Tuple2(
						_Utils_update(
							model,
							{
								history: A2($author$project$History$mapInPlace, $author$project$League$clearMatch, model.history),
								timeFilter: tf
							}),
						$elm$core$Platform$Cmd$none));
			case 'ReceivedTimeFilter':
				var raw = msg.a;
				var tf = A2(
					$elm$core$Maybe$withDefault,
					$author$project$Main$All,
					$author$project$Main$parseFilter(raw));
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{timeFilter: tf}),
					$elm$core$Platform$Cmd$none);
			default:
				var raw = msg.a;
				var ignoredIds = $elm$core$String$isEmpty(raw) ? $elm$core$Set$empty : $elm$core$Set$fromList(
					A2($elm$core$String$split, ',', raw));
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{ignoredPlayers: ignoredIds}),
					$elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Main$CancelPlayerDeletion = {$: 'CancelPlayerDeletion'};
var $author$project$Main$CancelTogglePlayerTime = {$: 'CancelTogglePlayerTime'};
var $author$project$Main$ConfirmPlayerDeletion = F2(
	function (a, b) {
		return {$: 'ConfirmPlayerDeletion', a: a, b: b};
	});
var $author$project$Main$ConfirmTogglePlayerTime = F3(
	function (a, b, c) {
		return {$: 'ConfirmTogglePlayerTime', a: a, b: b, c: c};
	});
var $author$project$Main$KeeperConfirmedAddPlayer = {$: 'KeeperConfirmedAddPlayer'};
var $author$project$Main$KeeperToggledAddPlayerAM = {$: 'KeeperToggledAddPlayerAM'};
var $author$project$Main$KeeperToggledAddPlayerPM = {$: 'KeeperToggledAddPlayerPM'};
var $author$project$Main$KeeperUpdatedAddPlayerName = function (a) {
	return {$: 'KeeperUpdatedAddPlayerName', a: a};
};
var $author$project$Main$KeeperUpdatedAddPlayerRating = function (a) {
	return {$: 'KeeperUpdatedAddPlayerRating', a: a};
};
var $author$project$Main$KeeperWantsToHideAddPlayerPopup = {$: 'KeeperWantsToHideAddPlayerPopup'};
var $rtfeldman$elm_css$Css$Preprocess$ApplyStyles = function (a) {
	return {$: 'ApplyStyles', a: a};
};
var $rtfeldman$elm_css$Css$Preprocess$AppendProperty = function (a) {
	return {$: 'AppendProperty', a: a};
};
var $rtfeldman$elm_css$Css$Structure$Property = function (a) {
	return {$: 'Property', a: a};
};
var $rtfeldman$elm_css$Css$Internal$property = F2(
	function (key, value) {
		return $rtfeldman$elm_css$Css$Preprocess$AppendProperty(
			$rtfeldman$elm_css$Css$Structure$Property(key + (':' + value)));
	});
var $rtfeldman$elm_css$Css$Internal$getOverloadedProperty = F3(
	function (functionName, desiredKey, style) {
		getOverloadedProperty:
		while (true) {
			switch (style.$) {
				case 'AppendProperty':
					var str = style.a.a;
					var key = A2(
						$elm$core$Maybe$withDefault,
						'',
						$elm$core$List$head(
							A2($elm$core$String$split, ':', str)));
					return A2($rtfeldman$elm_css$Css$Internal$property, desiredKey, key);
				case 'ExtendSelector':
					var selector = style.a;
					return A2($rtfeldman$elm_css$Css$Internal$property, desiredKey, 'elm-css-error-cannot-apply-' + (functionName + '-with-inapplicable-Style-for-selector'));
				case 'NestSnippet':
					var combinator = style.a;
					return A2($rtfeldman$elm_css$Css$Internal$property, desiredKey, 'elm-css-error-cannot-apply-' + (functionName + '-with-inapplicable-Style-for-combinator'));
				case 'WithPseudoElement':
					var pseudoElement = style.a;
					return A2($rtfeldman$elm_css$Css$Internal$property, desiredKey, 'elm-css-error-cannot-apply-' + (functionName + '-with-inapplicable-Style-for-pseudo-element setter'));
				case 'WithMedia':
					return A2($rtfeldman$elm_css$Css$Internal$property, desiredKey, 'elm-css-error-cannot-apply-' + (functionName + '-with-inapplicable-Style-for-media-query'));
				case 'WithKeyframes':
					return A2($rtfeldman$elm_css$Css$Internal$property, desiredKey, 'elm-css-error-cannot-apply-' + (functionName + '-with-inapplicable-Style-for-keyframes'));
				default:
					if (!style.a.b) {
						return A2($rtfeldman$elm_css$Css$Internal$property, desiredKey, 'elm-css-error-cannot-apply-' + (functionName + '-with-empty-Style'));
					} else {
						if (!style.a.b.b) {
							var _v1 = style.a;
							var only = _v1.a;
							var $temp$functionName = functionName,
								$temp$desiredKey = desiredKey,
								$temp$style = only;
							functionName = $temp$functionName;
							desiredKey = $temp$desiredKey;
							style = $temp$style;
							continue getOverloadedProperty;
						} else {
							var _v2 = style.a;
							var first = _v2.a;
							var rest = _v2.b;
							var $temp$functionName = functionName,
								$temp$desiredKey = desiredKey,
								$temp$style = $rtfeldman$elm_css$Css$Preprocess$ApplyStyles(rest);
							functionName = $temp$functionName;
							desiredKey = $temp$desiredKey;
							style = $temp$style;
							continue getOverloadedProperty;
						}
					}
			}
		}
	});
var $rtfeldman$elm_css$Css$Internal$IncompatibleUnits = {$: 'IncompatibleUnits'};
var $rtfeldman$elm_css$Css$Structure$Compatible = {$: 'Compatible'};
var $elm$core$String$fromFloat = _String_fromNumber;
var $rtfeldman$elm_css$Css$Internal$lengthConverter = F3(
	function (units, unitLabel, numericValue) {
		return {
			absoluteLength: $rtfeldman$elm_css$Css$Structure$Compatible,
			calc: $rtfeldman$elm_css$Css$Structure$Compatible,
			flexBasis: $rtfeldman$elm_css$Css$Structure$Compatible,
			fontSize: $rtfeldman$elm_css$Css$Structure$Compatible,
			length: $rtfeldman$elm_css$Css$Structure$Compatible,
			lengthOrAuto: $rtfeldman$elm_css$Css$Structure$Compatible,
			lengthOrAutoOrCoverOrContain: $rtfeldman$elm_css$Css$Structure$Compatible,
			lengthOrMinMaxDimension: $rtfeldman$elm_css$Css$Structure$Compatible,
			lengthOrNone: $rtfeldman$elm_css$Css$Structure$Compatible,
			lengthOrNoneOrMinMaxDimension: $rtfeldman$elm_css$Css$Structure$Compatible,
			lengthOrNumber: $rtfeldman$elm_css$Css$Structure$Compatible,
			lengthOrNumberOrAutoOrNoneOrContent: $rtfeldman$elm_css$Css$Structure$Compatible,
			numericValue: numericValue,
			textIndent: $rtfeldman$elm_css$Css$Structure$Compatible,
			unitLabel: unitLabel,
			units: units,
			value: _Utils_ap(
				$elm$core$String$fromFloat(numericValue),
				unitLabel)
		};
	});
var $rtfeldman$elm_css$Css$Internal$lengthForOverloadedProperty = A3($rtfeldman$elm_css$Css$Internal$lengthConverter, $rtfeldman$elm_css$Css$Internal$IncompatibleUnits, '', 0);
var $rtfeldman$elm_css$Css$alignItems = function (fn) {
	return A3(
		$rtfeldman$elm_css$Css$Internal$getOverloadedProperty,
		'alignItems',
		'align-items',
		fn($rtfeldman$elm_css$Css$Internal$lengthForOverloadedProperty));
};
var $rtfeldman$elm_css$Css$auto = {alignItemsOrAuto: $rtfeldman$elm_css$Css$Structure$Compatible, cursor: $rtfeldman$elm_css$Css$Structure$Compatible, flexBasis: $rtfeldman$elm_css$Css$Structure$Compatible, intOrAuto: $rtfeldman$elm_css$Css$Structure$Compatible, justifyContentOrAuto: $rtfeldman$elm_css$Css$Structure$Compatible, lengthOrAuto: $rtfeldman$elm_css$Css$Structure$Compatible, lengthOrAutoOrCoverOrContain: $rtfeldman$elm_css$Css$Structure$Compatible, lengthOrNumberOrAutoOrNoneOrContent: $rtfeldman$elm_css$Css$Structure$Compatible, overflow: $rtfeldman$elm_css$Css$Structure$Compatible, pointerEvents: $rtfeldman$elm_css$Css$Structure$Compatible, tableLayout: $rtfeldman$elm_css$Css$Structure$Compatible, textRendering: $rtfeldman$elm_css$Css$Structure$Compatible, touchAction: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'auto'};
var $rtfeldman$elm_css$Css$property = F2(
	function (key, value) {
		return $rtfeldman$elm_css$Css$Preprocess$AppendProperty(
			$rtfeldman$elm_css$Css$Structure$Property(key + (':' + value)));
	});
var $rtfeldman$elm_css$Css$backgroundColor = function (c) {
	return A2($rtfeldman$elm_css$Css$property, 'background-color', c.value);
};
var $rtfeldman$elm_css$Css$batch = $rtfeldman$elm_css$Css$Preprocess$ApplyStyles;
var $rtfeldman$elm_css$Css$block = {display: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'block'};
var $rtfeldman$elm_css$Css$prop1 = F2(
	function (key, arg) {
		return A2($rtfeldman$elm_css$Css$property, key, arg.value);
	});
var $rtfeldman$elm_css$Css$border = $rtfeldman$elm_css$Css$prop1('border');
var $rtfeldman$elm_css$Css$prop3 = F4(
	function (key, argA, argB, argC) {
		return A2($rtfeldman$elm_css$Css$property, key, argA.value + (' ' + (argB.value + (' ' + argC.value))));
	});
var $rtfeldman$elm_css$Css$border3 = $rtfeldman$elm_css$Css$prop3('border');
var $rtfeldman$elm_css$Css$borderBox = {backgroundClip: $rtfeldman$elm_css$Css$Structure$Compatible, boxSizing: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'border-box'};
var $rtfeldman$elm_css$Css$Structure$PseudoElement = function (a) {
	return {$: 'PseudoElement', a: a};
};
var $rtfeldman$elm_css$Css$Preprocess$WithPseudoElement = F2(
	function (a, b) {
		return {$: 'WithPseudoElement', a: a, b: b};
	});
var $rtfeldman$elm_css$Css$pseudoElement = function (element) {
	return $rtfeldman$elm_css$Css$Preprocess$WithPseudoElement(
		$rtfeldman$elm_css$Css$Structure$PseudoElement(element));
};
var $rtfeldman$elm_css$Css$after = $rtfeldman$elm_css$Css$pseudoElement('after');
var $rtfeldman$elm_css$Css$before = $rtfeldman$elm_css$Css$pseudoElement('before');
var $rtfeldman$elm_css$Css$boxSizing = $rtfeldman$elm_css$Css$prop1('box-sizing');
var $rtfeldman$elm_css$Css$Structure$UniversalSelectorSequence = function (a) {
	return {$: 'UniversalSelectorSequence', a: a};
};
var $rtfeldman$elm_css$Css$Structure$Selector = F3(
	function (a, b, c) {
		return {$: 'Selector', a: a, b: b, c: c};
	});
var $rtfeldman$elm_css$Css$Preprocess$Snippet = function (a) {
	return {$: 'Snippet', a: a};
};
var $rtfeldman$elm_css$Css$Preprocess$StyleBlock = F3(
	function (a, b, c) {
		return {$: 'StyleBlock', a: a, b: b, c: c};
	});
var $rtfeldman$elm_css$Css$Preprocess$StyleBlockDeclaration = function (a) {
	return {$: 'StyleBlockDeclaration', a: a};
};
var $rtfeldman$elm_css$VirtualDom$Styled$makeSnippet = F2(
	function (styles, sequence) {
		var selector = A3($rtfeldman$elm_css$Css$Structure$Selector, sequence, _List_Nil, $elm$core$Maybe$Nothing);
		return $rtfeldman$elm_css$Css$Preprocess$Snippet(
			_List_fromArray(
				[
					$rtfeldman$elm_css$Css$Preprocess$StyleBlockDeclaration(
					A3($rtfeldman$elm_css$Css$Preprocess$StyleBlock, selector, _List_Nil, styles))
				]));
	});
var $rtfeldman$elm_css$Css$Global$everything = function (styles) {
	return A2(
		$rtfeldman$elm_css$VirtualDom$Styled$makeSnippet,
		styles,
		$rtfeldman$elm_css$Css$Structure$UniversalSelectorSequence(_List_Nil));
};
var $elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $elm$core$List$all = F2(
	function (isOkay, list) {
		return !A2(
			$elm$core$List$any,
			A2($elm$core$Basics$composeL, $elm$core$Basics$not, isOkay),
			list);
	});
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $rtfeldman$elm_css$Css$Structure$compactHelp = F2(
	function (declaration, _v0) {
		var keyframesByName = _v0.a;
		var declarations = _v0.b;
		switch (declaration.$) {
			case 'StyleBlockDeclaration':
				var _v2 = declaration.a;
				var properties = _v2.c;
				return $elm$core$List$isEmpty(properties) ? _Utils_Tuple2(keyframesByName, declarations) : _Utils_Tuple2(
					keyframesByName,
					A2($elm$core$List$cons, declaration, declarations));
			case 'MediaRule':
				var styleBlocks = declaration.b;
				return A2(
					$elm$core$List$all,
					function (_v3) {
						var properties = _v3.c;
						return $elm$core$List$isEmpty(properties);
					},
					styleBlocks) ? _Utils_Tuple2(keyframesByName, declarations) : _Utils_Tuple2(
					keyframesByName,
					A2($elm$core$List$cons, declaration, declarations));
			case 'SupportsRule':
				var otherDeclarations = declaration.b;
				return $elm$core$List$isEmpty(otherDeclarations) ? _Utils_Tuple2(keyframesByName, declarations) : _Utils_Tuple2(
					keyframesByName,
					A2($elm$core$List$cons, declaration, declarations));
			case 'DocumentRule':
				return _Utils_Tuple2(
					keyframesByName,
					A2($elm$core$List$cons, declaration, declarations));
			case 'PageRule':
				var properties = declaration.a;
				return $elm$core$List$isEmpty(properties) ? _Utils_Tuple2(keyframesByName, declarations) : _Utils_Tuple2(
					keyframesByName,
					A2($elm$core$List$cons, declaration, declarations));
			case 'FontFace':
				var properties = declaration.a;
				return $elm$core$List$isEmpty(properties) ? _Utils_Tuple2(keyframesByName, declarations) : _Utils_Tuple2(
					keyframesByName,
					A2($elm$core$List$cons, declaration, declarations));
			case 'Keyframes':
				var record = declaration.a;
				return $elm$core$String$isEmpty(record.declaration) ? _Utils_Tuple2(keyframesByName, declarations) : _Utils_Tuple2(
					A3($elm$core$Dict$insert, record.name, record.declaration, keyframesByName),
					declarations);
			case 'Viewport':
				var properties = declaration.a;
				return $elm$core$List$isEmpty(properties) ? _Utils_Tuple2(keyframesByName, declarations) : _Utils_Tuple2(
					keyframesByName,
					A2($elm$core$List$cons, declaration, declarations));
			case 'CounterStyle':
				var properties = declaration.a;
				return $elm$core$List$isEmpty(properties) ? _Utils_Tuple2(keyframesByName, declarations) : _Utils_Tuple2(
					keyframesByName,
					A2($elm$core$List$cons, declaration, declarations));
			default:
				var tuples = declaration.a;
				return A2(
					$elm$core$List$all,
					function (_v4) {
						var properties = _v4.b;
						return $elm$core$List$isEmpty(properties);
					},
					tuples) ? _Utils_Tuple2(keyframesByName, declarations) : _Utils_Tuple2(
					keyframesByName,
					A2($elm$core$List$cons, declaration, declarations));
		}
	});
var $rtfeldman$elm_css$Css$Structure$Keyframes = function (a) {
	return {$: 'Keyframes', a: a};
};
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $rtfeldman$elm_css$Css$Structure$withKeyframeDeclarations = F2(
	function (keyframesByName, compactedDeclarations) {
		return A2(
			$elm$core$List$append,
			A2(
				$elm$core$List$map,
				function (_v0) {
					var name = _v0.a;
					var decl = _v0.b;
					return $rtfeldman$elm_css$Css$Structure$Keyframes(
						{declaration: decl, name: name});
				},
				$elm$core$Dict$toList(keyframesByName)),
			compactedDeclarations);
	});
var $rtfeldman$elm_css$Css$Structure$compactDeclarations = function (declarations) {
	var _v0 = A3(
		$elm$core$List$foldr,
		$rtfeldman$elm_css$Css$Structure$compactHelp,
		_Utils_Tuple2($elm$core$Dict$empty, _List_Nil),
		declarations);
	var keyframesByName = _v0.a;
	var compactedDeclarations = _v0.b;
	return A2($rtfeldman$elm_css$Css$Structure$withKeyframeDeclarations, keyframesByName, compactedDeclarations);
};
var $rtfeldman$elm_css$Css$Structure$compactStylesheet = function (_v0) {
	var charset = _v0.charset;
	var imports = _v0.imports;
	var namespaces = _v0.namespaces;
	var declarations = _v0.declarations;
	return {
		charset: charset,
		declarations: $rtfeldman$elm_css$Css$Structure$compactDeclarations(declarations),
		imports: imports,
		namespaces: namespaces
	};
};
var $rtfeldman$elm_css$Css$Structure$Output$charsetToString = function (charset) {
	return A2(
		$elm$core$Maybe$withDefault,
		'',
		A2(
			$elm$core$Maybe$map,
			function (str) {
				return '@charset \"' + (str + '\"');
			},
			charset));
};
var $rtfeldman$elm_css$Css$String$mapJoinHelp = F4(
	function (map, sep, strs, result) {
		mapJoinHelp:
		while (true) {
			if (!strs.b) {
				return result;
			} else {
				if (!strs.b.b) {
					var first = strs.a;
					return result + (map(first) + '');
				} else {
					var first = strs.a;
					var rest = strs.b;
					var $temp$map = map,
						$temp$sep = sep,
						$temp$strs = rest,
						$temp$result = result + (map(first) + (sep + ''));
					map = $temp$map;
					sep = $temp$sep;
					strs = $temp$strs;
					result = $temp$result;
					continue mapJoinHelp;
				}
			}
		}
	});
var $rtfeldman$elm_css$Css$String$mapJoin = F3(
	function (map, sep, strs) {
		return A4($rtfeldman$elm_css$Css$String$mapJoinHelp, map, sep, strs, '');
	});
var $rtfeldman$elm_css$Css$Structure$Output$mediaExpressionToString = function (expression) {
	return '(' + (expression.feature + (A2(
		$elm$core$Maybe$withDefault,
		'',
		A2(
			$elm$core$Maybe$map,
			$elm$core$Basics$append(': '),
			expression.value)) + ')'));
};
var $rtfeldman$elm_css$Css$Structure$Output$mediaTypeToString = function (mediaType) {
	switch (mediaType.$) {
		case 'Print':
			return 'print';
		case 'Screen':
			return 'screen';
		default:
			return 'speech';
	}
};
var $rtfeldman$elm_css$Css$Structure$Output$mediaQueryToString = function (mediaQuery) {
	var prefixWith = F3(
		function (str, mediaType, expressions) {
			return str + (' ' + A2(
				$elm$core$String$join,
				' and ',
				A2(
					$elm$core$List$cons,
					$rtfeldman$elm_css$Css$Structure$Output$mediaTypeToString(mediaType),
					A2($elm$core$List$map, $rtfeldman$elm_css$Css$Structure$Output$mediaExpressionToString, expressions))));
		});
	switch (mediaQuery.$) {
		case 'AllQuery':
			var expressions = mediaQuery.a;
			return A3($rtfeldman$elm_css$Css$String$mapJoin, $rtfeldman$elm_css$Css$Structure$Output$mediaExpressionToString, ' and ', expressions);
		case 'OnlyQuery':
			var mediaType = mediaQuery.a;
			var expressions = mediaQuery.b;
			return A3(prefixWith, 'only', mediaType, expressions);
		case 'NotQuery':
			var mediaType = mediaQuery.a;
			var expressions = mediaQuery.b;
			return A3(prefixWith, 'not', mediaType, expressions);
		default:
			var str = mediaQuery.a;
			return str;
	}
};
var $rtfeldman$elm_css$Css$Structure$Output$importMediaQueryToString = F2(
	function (name, mediaQuery) {
		return '@import \"' + (name + ($rtfeldman$elm_css$Css$Structure$Output$mediaQueryToString(mediaQuery) + '\"'));
	});
var $rtfeldman$elm_css$Css$Structure$Output$importToString = function (_v0) {
	var name = _v0.a;
	var mediaQueries = _v0.b;
	return A3(
		$rtfeldman$elm_css$Css$String$mapJoin,
		$rtfeldman$elm_css$Css$Structure$Output$importMediaQueryToString(name),
		'\n',
		mediaQueries);
};
var $rtfeldman$elm_css$Css$Structure$Output$namespaceToString = function (_v0) {
	var prefix = _v0.a;
	var str = _v0.b;
	return '@namespace ' + (prefix + ('\"' + (str + '\"')));
};
var $rtfeldman$elm_css$Css$Structure$Output$emitProperties = function (properties) {
	return A3(
		$rtfeldman$elm_css$Css$String$mapJoin,
		function (_v0) {
			var prop = _v0.a;
			return prop + ';';
		},
		'',
		properties);
};
var $elm$core$String$append = _String_append;
var $rtfeldman$elm_css$Css$Structure$Output$pseudoElementToString = function (_v0) {
	var str = _v0.a;
	return '::' + str;
};
var $rtfeldman$elm_css$Css$Structure$Output$combinatorToString = function (combinator) {
	switch (combinator.$) {
		case 'AdjacentSibling':
			return '+';
		case 'GeneralSibling':
			return '~';
		case 'Child':
			return '>';
		default:
			return '';
	}
};
var $rtfeldman$elm_css$Css$Structure$Output$repeatableSimpleSelectorToString = function (repeatableSimpleSelector) {
	switch (repeatableSimpleSelector.$) {
		case 'ClassSelector':
			var str = repeatableSimpleSelector.a;
			return '.' + str;
		case 'IdSelector':
			var str = repeatableSimpleSelector.a;
			return '#' + str;
		case 'PseudoClassSelector':
			var str = repeatableSimpleSelector.a;
			return ':' + str;
		default:
			var str = repeatableSimpleSelector.a;
			return '[' + (str + ']');
	}
};
var $rtfeldman$elm_css$Css$Structure$Output$simpleSelectorSequenceToString = function (simpleSelectorSequence) {
	switch (simpleSelectorSequence.$) {
		case 'TypeSelectorSequence':
			var str = simpleSelectorSequence.a.a;
			var repeatableSimpleSelectors = simpleSelectorSequence.b;
			return _Utils_ap(
				str,
				A3($rtfeldman$elm_css$Css$String$mapJoin, $rtfeldman$elm_css$Css$Structure$Output$repeatableSimpleSelectorToString, '', repeatableSimpleSelectors));
		case 'UniversalSelectorSequence':
			var repeatableSimpleSelectors = simpleSelectorSequence.a;
			return $elm$core$List$isEmpty(repeatableSimpleSelectors) ? '*' : A3($rtfeldman$elm_css$Css$String$mapJoin, $rtfeldman$elm_css$Css$Structure$Output$repeatableSimpleSelectorToString, '', repeatableSimpleSelectors);
		default:
			var str = simpleSelectorSequence.a;
			var repeatableSimpleSelectors = simpleSelectorSequence.b;
			return _Utils_ap(
				str,
				A3($rtfeldman$elm_css$Css$String$mapJoin, $rtfeldman$elm_css$Css$Structure$Output$repeatableSimpleSelectorToString, '', repeatableSimpleSelectors));
	}
};
var $rtfeldman$elm_css$Css$Structure$Output$selectorChainToString = function (_v0) {
	var combinator = _v0.a;
	var sequence = _v0.b;
	return $rtfeldman$elm_css$Css$Structure$Output$combinatorToString(combinator) + (' ' + $rtfeldman$elm_css$Css$Structure$Output$simpleSelectorSequenceToString(sequence));
};
var $rtfeldman$elm_css$Css$Structure$Output$selectorToString = function (_v0) {
	var simpleSelectorSequence = _v0.a;
	var chain = _v0.b;
	var pseudoElement = _v0.c;
	var segments = A2(
		$elm$core$List$cons,
		$rtfeldman$elm_css$Css$Structure$Output$simpleSelectorSequenceToString(simpleSelectorSequence),
		A2($elm$core$List$map, $rtfeldman$elm_css$Css$Structure$Output$selectorChainToString, chain));
	var pseudoElementsString = A2(
		$elm$core$Maybe$withDefault,
		'',
		A2($elm$core$Maybe$map, $rtfeldman$elm_css$Css$Structure$Output$pseudoElementToString, pseudoElement));
	return A2(
		$elm$core$String$append,
		A2($elm$core$String$join, ' ', segments),
		pseudoElementsString);
};
var $rtfeldman$elm_css$Css$Structure$Output$prettyPrintStyleBlock = function (_v0) {
	var firstSelector = _v0.a;
	var otherSelectors = _v0.b;
	var properties = _v0.c;
	var selectorStr = A3(
		$rtfeldman$elm_css$Css$String$mapJoin,
		$rtfeldman$elm_css$Css$Structure$Output$selectorToString,
		',',
		A2($elm$core$List$cons, firstSelector, otherSelectors));
	return selectorStr + ('{' + ($rtfeldman$elm_css$Css$Structure$Output$emitProperties(properties) + '}'));
};
var $rtfeldman$elm_css$Css$Structure$Output$prettyPrintDeclaration = function (decl) {
	switch (decl.$) {
		case 'StyleBlockDeclaration':
			var styleBlock = decl.a;
			return $rtfeldman$elm_css$Css$Structure$Output$prettyPrintStyleBlock(styleBlock);
		case 'MediaRule':
			var mediaQueries = decl.a;
			var styleBlocks = decl.b;
			var query = A3($rtfeldman$elm_css$Css$String$mapJoin, $rtfeldman$elm_css$Css$Structure$Output$mediaQueryToString, ', ', mediaQueries);
			var blocks = A3($rtfeldman$elm_css$Css$String$mapJoin, $rtfeldman$elm_css$Css$Structure$Output$prettyPrintStyleBlock, '\n', styleBlocks);
			return '@media ' + (query + ('{' + (blocks + '}')));
		case 'SupportsRule':
			return 'TODO';
		case 'DocumentRule':
			return 'TODO';
		case 'PageRule':
			return 'TODO';
		case 'FontFace':
			return 'TODO';
		case 'Keyframes':
			var name = decl.a.name;
			var declaration = decl.a.declaration;
			return '@keyframes ' + (name + ('{' + (declaration + '}')));
		case 'Viewport':
			return 'TODO';
		case 'CounterStyle':
			return 'TODO';
		default:
			return 'TODO';
	}
};
var $rtfeldman$elm_css$Css$Structure$Output$prettyPrint = function (_v0) {
	var charset = _v0.charset;
	var imports = _v0.imports;
	var namespaces = _v0.namespaces;
	var declarations = _v0.declarations;
	return $rtfeldman$elm_css$Css$Structure$Output$charsetToString(charset) + (A3($rtfeldman$elm_css$Css$String$mapJoin, $rtfeldman$elm_css$Css$Structure$Output$importToString, '\n', imports) + (A3($rtfeldman$elm_css$Css$String$mapJoin, $rtfeldman$elm_css$Css$Structure$Output$namespaceToString, '\n', namespaces) + (A3($rtfeldman$elm_css$Css$String$mapJoin, $rtfeldman$elm_css$Css$Structure$Output$prettyPrintDeclaration, '\n', declarations) + '')));
};
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $elm$core$List$concatMap = F2(
	function (f, list) {
		return $elm$core$List$concat(
			A2($elm$core$List$map, f, list));
	});
var $rtfeldman$elm_css$Css$Structure$CounterStyle = function (a) {
	return {$: 'CounterStyle', a: a};
};
var $rtfeldman$elm_css$Css$Structure$FontFace = function (a) {
	return {$: 'FontFace', a: a};
};
var $rtfeldman$elm_css$Css$Structure$PageRule = function (a) {
	return {$: 'PageRule', a: a};
};
var $rtfeldman$elm_css$Css$Structure$StyleBlock = F3(
	function (a, b, c) {
		return {$: 'StyleBlock', a: a, b: b, c: c};
	});
var $rtfeldman$elm_css$Css$Structure$StyleBlockDeclaration = function (a) {
	return {$: 'StyleBlockDeclaration', a: a};
};
var $rtfeldman$elm_css$Css$Structure$SupportsRule = F2(
	function (a, b) {
		return {$: 'SupportsRule', a: a, b: b};
	});
var $rtfeldman$elm_css$Css$Structure$Viewport = function (a) {
	return {$: 'Viewport', a: a};
};
var $rtfeldman$elm_css$Css$Structure$MediaRule = F2(
	function (a, b) {
		return {$: 'MediaRule', a: a, b: b};
	});
var $rtfeldman$elm_css$Css$Structure$mapLast = F2(
	function (update, list) {
		if (!list.b) {
			return list;
		} else {
			if (!list.b.b) {
				var only = list.a;
				return _List_fromArray(
					[
						update(only)
					]);
			} else {
				var first = list.a;
				var rest = list.b;
				return A2(
					$elm$core$List$cons,
					first,
					A2($rtfeldman$elm_css$Css$Structure$mapLast, update, rest));
			}
		}
	});
var $rtfeldman$elm_css$Css$Structure$withPropertyAppended = F2(
	function (property, _v0) {
		var firstSelector = _v0.a;
		var otherSelectors = _v0.b;
		var properties = _v0.c;
		return A3(
			$rtfeldman$elm_css$Css$Structure$StyleBlock,
			firstSelector,
			otherSelectors,
			_Utils_ap(
				properties,
				_List_fromArray(
					[property])));
	});
var $rtfeldman$elm_css$Css$Structure$appendProperty = F2(
	function (property, declarations) {
		if (!declarations.b) {
			return declarations;
		} else {
			if (!declarations.b.b) {
				switch (declarations.a.$) {
					case 'StyleBlockDeclaration':
						var styleBlock = declarations.a.a;
						return _List_fromArray(
							[
								$rtfeldman$elm_css$Css$Structure$StyleBlockDeclaration(
								A2($rtfeldman$elm_css$Css$Structure$withPropertyAppended, property, styleBlock))
							]);
					case 'MediaRule':
						var _v1 = declarations.a;
						var mediaQueries = _v1.a;
						var styleBlocks = _v1.b;
						return _List_fromArray(
							[
								A2(
								$rtfeldman$elm_css$Css$Structure$MediaRule,
								mediaQueries,
								A2(
									$rtfeldman$elm_css$Css$Structure$mapLast,
									$rtfeldman$elm_css$Css$Structure$withPropertyAppended(property),
									styleBlocks))
							]);
					default:
						return declarations;
				}
			} else {
				var first = declarations.a;
				var rest = declarations.b;
				return A2(
					$elm$core$List$cons,
					first,
					A2($rtfeldman$elm_css$Css$Structure$appendProperty, property, rest));
			}
		}
	});
var $rtfeldman$elm_css$Css$Structure$appendToLastSelector = F2(
	function (f, styleBlock) {
		if (!styleBlock.b.b) {
			var only = styleBlock.a;
			var properties = styleBlock.c;
			return _List_fromArray(
				[
					A3($rtfeldman$elm_css$Css$Structure$StyleBlock, only, _List_Nil, properties),
					A3(
					$rtfeldman$elm_css$Css$Structure$StyleBlock,
					f(only),
					_List_Nil,
					_List_Nil)
				]);
		} else {
			var first = styleBlock.a;
			var rest = styleBlock.b;
			var properties = styleBlock.c;
			var newRest = A2($elm$core$List$map, f, rest);
			var newFirst = f(first);
			return _List_fromArray(
				[
					A3($rtfeldman$elm_css$Css$Structure$StyleBlock, first, rest, properties),
					A3($rtfeldman$elm_css$Css$Structure$StyleBlock, newFirst, newRest, _List_Nil)
				]);
		}
	});
var $rtfeldman$elm_css$Css$Structure$applyPseudoElement = F2(
	function (pseudo, _v0) {
		var sequence = _v0.a;
		var selectors = _v0.b;
		return A3(
			$rtfeldman$elm_css$Css$Structure$Selector,
			sequence,
			selectors,
			$elm$core$Maybe$Just(pseudo));
	});
var $rtfeldman$elm_css$Css$Structure$appendPseudoElementToLastSelector = F2(
	function (pseudo, styleBlock) {
		return A2(
			$rtfeldman$elm_css$Css$Structure$appendToLastSelector,
			$rtfeldman$elm_css$Css$Structure$applyPseudoElement(pseudo),
			styleBlock);
	});
var $rtfeldman$elm_css$Css$Structure$CustomSelector = F2(
	function (a, b) {
		return {$: 'CustomSelector', a: a, b: b};
	});
var $rtfeldman$elm_css$Css$Structure$TypeSelectorSequence = F2(
	function (a, b) {
		return {$: 'TypeSelectorSequence', a: a, b: b};
	});
var $rtfeldman$elm_css$Css$Structure$appendRepeatable = F2(
	function (selector, sequence) {
		switch (sequence.$) {
			case 'TypeSelectorSequence':
				var typeSelector = sequence.a;
				var list = sequence.b;
				return A2(
					$rtfeldman$elm_css$Css$Structure$TypeSelectorSequence,
					typeSelector,
					_Utils_ap(
						list,
						_List_fromArray(
							[selector])));
			case 'UniversalSelectorSequence':
				var list = sequence.a;
				return $rtfeldman$elm_css$Css$Structure$UniversalSelectorSequence(
					_Utils_ap(
						list,
						_List_fromArray(
							[selector])));
			default:
				var str = sequence.a;
				var list = sequence.b;
				return A2(
					$rtfeldman$elm_css$Css$Structure$CustomSelector,
					str,
					_Utils_ap(
						list,
						_List_fromArray(
							[selector])));
		}
	});
var $rtfeldman$elm_css$Css$Structure$appendRepeatableWithCombinator = F2(
	function (selector, list) {
		if (!list.b) {
			return _List_Nil;
		} else {
			if (!list.b.b) {
				var _v1 = list.a;
				var combinator = _v1.a;
				var sequence = _v1.b;
				return _List_fromArray(
					[
						_Utils_Tuple2(
						combinator,
						A2($rtfeldman$elm_css$Css$Structure$appendRepeatable, selector, sequence))
					]);
			} else {
				var first = list.a;
				var rest = list.b;
				return A2(
					$elm$core$List$cons,
					first,
					A2($rtfeldman$elm_css$Css$Structure$appendRepeatableWithCombinator, selector, rest));
			}
		}
	});
var $rtfeldman$elm_css$Css$Structure$appendRepeatableSelector = F2(
	function (repeatableSimpleSelector, selector) {
		if (!selector.b.b) {
			var sequence = selector.a;
			var pseudoElement = selector.c;
			return A3(
				$rtfeldman$elm_css$Css$Structure$Selector,
				A2($rtfeldman$elm_css$Css$Structure$appendRepeatable, repeatableSimpleSelector, sequence),
				_List_Nil,
				pseudoElement);
		} else {
			var firstSelector = selector.a;
			var tuples = selector.b;
			var pseudoElement = selector.c;
			return A3(
				$rtfeldman$elm_css$Css$Structure$Selector,
				firstSelector,
				A2($rtfeldman$elm_css$Css$Structure$appendRepeatableWithCombinator, repeatableSimpleSelector, tuples),
				pseudoElement);
		}
	});
var $rtfeldman$elm_css$Css$Structure$appendRepeatableToLastSelector = F2(
	function (selector, styleBlock) {
		return A2(
			$rtfeldman$elm_css$Css$Structure$appendToLastSelector,
			$rtfeldman$elm_css$Css$Structure$appendRepeatableSelector(selector),
			styleBlock);
	});
var $rtfeldman$elm_css$Css$Preprocess$Resolve$collectSelectors = function (declarations) {
	collectSelectors:
	while (true) {
		if (!declarations.b) {
			return _List_Nil;
		} else {
			if (declarations.a.$ === 'StyleBlockDeclaration') {
				var _v1 = declarations.a.a;
				var firstSelector = _v1.a;
				var otherSelectors = _v1.b;
				var rest = declarations.b;
				return _Utils_ap(
					A2($elm$core$List$cons, firstSelector, otherSelectors),
					$rtfeldman$elm_css$Css$Preprocess$Resolve$collectSelectors(rest));
			} else {
				var rest = declarations.b;
				var $temp$declarations = rest;
				declarations = $temp$declarations;
				continue collectSelectors;
			}
		}
	}
};
var $rtfeldman$elm_css$Css$Structure$DocumentRule = F5(
	function (a, b, c, d, e) {
		return {$: 'DocumentRule', a: a, b: b, c: c, d: d, e: e};
	});
var $rtfeldman$elm_css$Css$Structure$concatMapLastStyleBlock = F2(
	function (update, declarations) {
		_v0$12:
		while (true) {
			if (!declarations.b) {
				return declarations;
			} else {
				if (!declarations.b.b) {
					switch (declarations.a.$) {
						case 'StyleBlockDeclaration':
							var styleBlock = declarations.a.a;
							return A2(
								$elm$core$List$map,
								$rtfeldman$elm_css$Css$Structure$StyleBlockDeclaration,
								update(styleBlock));
						case 'MediaRule':
							if (declarations.a.b.b) {
								if (!declarations.a.b.b.b) {
									var _v1 = declarations.a;
									var mediaQueries = _v1.a;
									var _v2 = _v1.b;
									var styleBlock = _v2.a;
									return _List_fromArray(
										[
											A2(
											$rtfeldman$elm_css$Css$Structure$MediaRule,
											mediaQueries,
											update(styleBlock))
										]);
								} else {
									var _v3 = declarations.a;
									var mediaQueries = _v3.a;
									var _v4 = _v3.b;
									var first = _v4.a;
									var rest = _v4.b;
									var _v5 = A2(
										$rtfeldman$elm_css$Css$Structure$concatMapLastStyleBlock,
										update,
										_List_fromArray(
											[
												A2($rtfeldman$elm_css$Css$Structure$MediaRule, mediaQueries, rest)
											]));
									if ((_v5.b && (_v5.a.$ === 'MediaRule')) && (!_v5.b.b)) {
										var _v6 = _v5.a;
										var newMediaQueries = _v6.a;
										var newStyleBlocks = _v6.b;
										return _List_fromArray(
											[
												A2(
												$rtfeldman$elm_css$Css$Structure$MediaRule,
												newMediaQueries,
												A2($elm$core$List$cons, first, newStyleBlocks))
											]);
									} else {
										var newDeclarations = _v5;
										return newDeclarations;
									}
								}
							} else {
								break _v0$12;
							}
						case 'SupportsRule':
							var _v7 = declarations.a;
							var str = _v7.a;
							var nestedDeclarations = _v7.b;
							return _List_fromArray(
								[
									A2(
									$rtfeldman$elm_css$Css$Structure$SupportsRule,
									str,
									A2($rtfeldman$elm_css$Css$Structure$concatMapLastStyleBlock, update, nestedDeclarations))
								]);
						case 'DocumentRule':
							var _v8 = declarations.a;
							var str1 = _v8.a;
							var str2 = _v8.b;
							var str3 = _v8.c;
							var str4 = _v8.d;
							var styleBlock = _v8.e;
							return A2(
								$elm$core$List$map,
								A4($rtfeldman$elm_css$Css$Structure$DocumentRule, str1, str2, str3, str4),
								update(styleBlock));
						case 'PageRule':
							return declarations;
						case 'FontFace':
							return declarations;
						case 'Keyframes':
							return declarations;
						case 'Viewport':
							return declarations;
						case 'CounterStyle':
							return declarations;
						default:
							return declarations;
					}
				} else {
					break _v0$12;
				}
			}
		}
		var first = declarations.a;
		var rest = declarations.b;
		return A2(
			$elm$core$List$cons,
			first,
			A2($rtfeldman$elm_css$Css$Structure$concatMapLastStyleBlock, update, rest));
	});
var $robinheghan$murmur3$Murmur3$HashData = F4(
	function (shift, seed, hash, charsProcessed) {
		return {charsProcessed: charsProcessed, hash: hash, seed: seed, shift: shift};
	});
var $robinheghan$murmur3$Murmur3$c1 = 3432918353;
var $robinheghan$murmur3$Murmur3$c2 = 461845907;
var $elm$core$Bitwise$shiftLeftBy = _Bitwise_shiftLeftBy;
var $robinheghan$murmur3$Murmur3$multiplyBy = F2(
	function (b, a) {
		return ((a & 65535) * b) + ((((a >>> 16) * b) & 65535) << 16);
	});
var $elm$core$Bitwise$or = _Bitwise_or;
var $robinheghan$murmur3$Murmur3$rotlBy = F2(
	function (b, a) {
		return (a << b) | (a >>> (32 - b));
	});
var $robinheghan$murmur3$Murmur3$finalize = function (data) {
	var acc = (!(!data.hash)) ? (data.seed ^ A2(
		$robinheghan$murmur3$Murmur3$multiplyBy,
		$robinheghan$murmur3$Murmur3$c2,
		A2(
			$robinheghan$murmur3$Murmur3$rotlBy,
			15,
			A2($robinheghan$murmur3$Murmur3$multiplyBy, $robinheghan$murmur3$Murmur3$c1, data.hash)))) : data.seed;
	var h0 = acc ^ data.charsProcessed;
	var h1 = A2($robinheghan$murmur3$Murmur3$multiplyBy, 2246822507, h0 ^ (h0 >>> 16));
	var h2 = A2($robinheghan$murmur3$Murmur3$multiplyBy, 3266489909, h1 ^ (h1 >>> 13));
	return (h2 ^ (h2 >>> 16)) >>> 0;
};
var $elm$core$String$foldl = _String_foldl;
var $robinheghan$murmur3$Murmur3$mix = F2(
	function (h1, k1) {
		return A2(
			$robinheghan$murmur3$Murmur3$multiplyBy,
			5,
			A2(
				$robinheghan$murmur3$Murmur3$rotlBy,
				13,
				h1 ^ A2(
					$robinheghan$murmur3$Murmur3$multiplyBy,
					$robinheghan$murmur3$Murmur3$c2,
					A2(
						$robinheghan$murmur3$Murmur3$rotlBy,
						15,
						A2($robinheghan$murmur3$Murmur3$multiplyBy, $robinheghan$murmur3$Murmur3$c1, k1))))) + 3864292196;
	});
var $robinheghan$murmur3$Murmur3$hashFold = F2(
	function (c, data) {
		var res = data.hash | ((255 & $elm$core$Char$toCode(c)) << data.shift);
		var _v0 = data.shift;
		if (_v0 === 24) {
			return {
				charsProcessed: data.charsProcessed + 1,
				hash: 0,
				seed: A2($robinheghan$murmur3$Murmur3$mix, data.seed, res),
				shift: 0
			};
		} else {
			return {charsProcessed: data.charsProcessed + 1, hash: res, seed: data.seed, shift: data.shift + 8};
		}
	});
var $robinheghan$murmur3$Murmur3$hashString = F2(
	function (seed, str) {
		return $robinheghan$murmur3$Murmur3$finalize(
			A3(
				$elm$core$String$foldl,
				$robinheghan$murmur3$Murmur3$hashFold,
				A4($robinheghan$murmur3$Murmur3$HashData, 0, seed, 0, 0),
				str));
	});
var $rtfeldman$elm_css$Hash$initialSeed = 15739;
var $elm$core$String$fromList = _String_fromList;
var $rtfeldman$elm_hex$Hex$unsafeToDigit = function (num) {
	unsafeToDigit:
	while (true) {
		switch (num) {
			case 0:
				return _Utils_chr('0');
			case 1:
				return _Utils_chr('1');
			case 2:
				return _Utils_chr('2');
			case 3:
				return _Utils_chr('3');
			case 4:
				return _Utils_chr('4');
			case 5:
				return _Utils_chr('5');
			case 6:
				return _Utils_chr('6');
			case 7:
				return _Utils_chr('7');
			case 8:
				return _Utils_chr('8');
			case 9:
				return _Utils_chr('9');
			case 10:
				return _Utils_chr('a');
			case 11:
				return _Utils_chr('b');
			case 12:
				return _Utils_chr('c');
			case 13:
				return _Utils_chr('d');
			case 14:
				return _Utils_chr('e');
			case 15:
				return _Utils_chr('f');
			default:
				var $temp$num = num;
				num = $temp$num;
				continue unsafeToDigit;
		}
	}
};
var $rtfeldman$elm_hex$Hex$unsafePositiveToDigits = F2(
	function (digits, num) {
		unsafePositiveToDigits:
		while (true) {
			if (num < 16) {
				return A2(
					$elm$core$List$cons,
					$rtfeldman$elm_hex$Hex$unsafeToDigit(num),
					digits);
			} else {
				var $temp$digits = A2(
					$elm$core$List$cons,
					$rtfeldman$elm_hex$Hex$unsafeToDigit(
						A2($elm$core$Basics$modBy, 16, num)),
					digits),
					$temp$num = (num / 16) | 0;
				digits = $temp$digits;
				num = $temp$num;
				continue unsafePositiveToDigits;
			}
		}
	});
var $rtfeldman$elm_hex$Hex$toString = function (num) {
	return $elm$core$String$fromList(
		(num < 0) ? A2(
			$elm$core$List$cons,
			_Utils_chr('-'),
			A2($rtfeldman$elm_hex$Hex$unsafePositiveToDigits, _List_Nil, -num)) : A2($rtfeldman$elm_hex$Hex$unsafePositiveToDigits, _List_Nil, num));
};
var $rtfeldman$elm_css$Hash$fromString = function (str) {
	return A2(
		$elm$core$String$cons,
		_Utils_chr('_'),
		$rtfeldman$elm_hex$Hex$toString(
			A2($robinheghan$murmur3$Murmur3$hashString, $rtfeldman$elm_css$Hash$initialSeed, str)));
};
var $rtfeldman$elm_css$Css$Preprocess$Resolve$last = function (list) {
	last:
	while (true) {
		if (!list.b) {
			return $elm$core$Maybe$Nothing;
		} else {
			if (!list.b.b) {
				var singleton = list.a;
				return $elm$core$Maybe$Just(singleton);
			} else {
				var rest = list.b;
				var $temp$list = rest;
				list = $temp$list;
				continue last;
			}
		}
	}
};
var $rtfeldman$elm_css$Css$Preprocess$Resolve$lastDeclaration = function (declarations) {
	lastDeclaration:
	while (true) {
		if (!declarations.b) {
			return $elm$core$Maybe$Nothing;
		} else {
			if (!declarations.b.b) {
				var x = declarations.a;
				return $elm$core$Maybe$Just(
					_List_fromArray(
						[x]));
			} else {
				var xs = declarations.b;
				var $temp$declarations = xs;
				declarations = $temp$declarations;
				continue lastDeclaration;
			}
		}
	}
};
var $rtfeldman$elm_css$Css$Preprocess$Resolve$oneOf = function (maybes) {
	oneOf:
	while (true) {
		if (!maybes.b) {
			return $elm$core$Maybe$Nothing;
		} else {
			var maybe = maybes.a;
			var rest = maybes.b;
			if (maybe.$ === 'Nothing') {
				var $temp$maybes = rest;
				maybes = $temp$maybes;
				continue oneOf;
			} else {
				return maybe;
			}
		}
	}
};
var $rtfeldman$elm_css$Css$Structure$FontFeatureValues = function (a) {
	return {$: 'FontFeatureValues', a: a};
};
var $rtfeldman$elm_css$Css$Preprocess$Resolve$resolveFontFeatureValues = function (tuples) {
	var expandTuples = function (tuplesToExpand) {
		if (!tuplesToExpand.b) {
			return _List_Nil;
		} else {
			var properties = tuplesToExpand.a;
			var rest = tuplesToExpand.b;
			return A2(
				$elm$core$List$cons,
				properties,
				expandTuples(rest));
		}
	};
	var newTuples = expandTuples(tuples);
	return _List_fromArray(
		[
			$rtfeldman$elm_css$Css$Structure$FontFeatureValues(newTuples)
		]);
};
var $elm$core$List$singleton = function (value) {
	return _List_fromArray(
		[value]);
};
var $rtfeldman$elm_css$Css$Structure$styleBlockToMediaRule = F2(
	function (mediaQueries, declaration) {
		if (declaration.$ === 'StyleBlockDeclaration') {
			var styleBlock = declaration.a;
			return A2(
				$rtfeldman$elm_css$Css$Structure$MediaRule,
				mediaQueries,
				_List_fromArray(
					[styleBlock]));
		} else {
			return declaration;
		}
	});
var $elm$core$List$tail = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(xs);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $rtfeldman$elm_css$Css$Preprocess$Resolve$toDocumentRule = F5(
	function (str1, str2, str3, str4, declaration) {
		if (declaration.$ === 'StyleBlockDeclaration') {
			var structureStyleBlock = declaration.a;
			return A5($rtfeldman$elm_css$Css$Structure$DocumentRule, str1, str2, str3, str4, structureStyleBlock);
		} else {
			return declaration;
		}
	});
var $rtfeldman$elm_css$Css$Preprocess$Resolve$toMediaRule = F2(
	function (mediaQueries, declaration) {
		switch (declaration.$) {
			case 'StyleBlockDeclaration':
				var structureStyleBlock = declaration.a;
				return A2(
					$rtfeldman$elm_css$Css$Structure$MediaRule,
					mediaQueries,
					_List_fromArray(
						[structureStyleBlock]));
			case 'MediaRule':
				var newMediaQueries = declaration.a;
				var structureStyleBlocks = declaration.b;
				return A2(
					$rtfeldman$elm_css$Css$Structure$MediaRule,
					_Utils_ap(mediaQueries, newMediaQueries),
					structureStyleBlocks);
			case 'SupportsRule':
				var str = declaration.a;
				var declarations = declaration.b;
				return A2(
					$rtfeldman$elm_css$Css$Structure$SupportsRule,
					str,
					A2(
						$elm$core$List$map,
						$rtfeldman$elm_css$Css$Preprocess$Resolve$toMediaRule(mediaQueries),
						declarations));
			case 'DocumentRule':
				var str1 = declaration.a;
				var str2 = declaration.b;
				var str3 = declaration.c;
				var str4 = declaration.d;
				var structureStyleBlock = declaration.e;
				return A5($rtfeldman$elm_css$Css$Structure$DocumentRule, str1, str2, str3, str4, structureStyleBlock);
			case 'PageRule':
				return declaration;
			case 'FontFace':
				return declaration;
			case 'Keyframes':
				return declaration;
			case 'Viewport':
				return declaration;
			case 'CounterStyle':
				return declaration;
			default:
				return declaration;
		}
	});
var $rtfeldman$elm_css$Css$Preprocess$unwrapSnippet = function (_v0) {
	var declarations = _v0.a;
	return declarations;
};
var $rtfeldman$elm_css$Css$Preprocess$Resolve$applyNestedStylesToLast = F4(
	function (nestedStyles, rest, f, declarations) {
		var withoutParent = function (decls) {
			return A2(
				$elm$core$Maybe$withDefault,
				_List_Nil,
				$elm$core$List$tail(decls));
		};
		var nextResult = A2(
			$rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles,
			rest,
			A2(
				$elm$core$Maybe$withDefault,
				_List_Nil,
				$rtfeldman$elm_css$Css$Preprocess$Resolve$lastDeclaration(declarations)));
		var newDeclarations = function () {
			var _v14 = _Utils_Tuple2(
				$elm$core$List$head(nextResult),
				$rtfeldman$elm_css$Css$Preprocess$Resolve$last(declarations));
			if ((_v14.a.$ === 'Just') && (_v14.b.$ === 'Just')) {
				var nextResultParent = _v14.a.a;
				var originalParent = _v14.b.a;
				return _Utils_ap(
					A2(
						$elm$core$List$take,
						$elm$core$List$length(declarations) - 1,
						declarations),
					_List_fromArray(
						[
							(!_Utils_eq(originalParent, nextResultParent)) ? nextResultParent : originalParent
						]));
			} else {
				return declarations;
			}
		}();
		var insertStylesToNestedDecl = function (lastDecl) {
			return $elm$core$List$concat(
				A2(
					$rtfeldman$elm_css$Css$Structure$mapLast,
					$rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles(nestedStyles),
					A2(
						$elm$core$List$map,
						$elm$core$List$singleton,
						A2($rtfeldman$elm_css$Css$Structure$concatMapLastStyleBlock, f, lastDecl))));
		};
		var initialResult = A2(
			$elm$core$Maybe$withDefault,
			_List_Nil,
			A2(
				$elm$core$Maybe$map,
				insertStylesToNestedDecl,
				$rtfeldman$elm_css$Css$Preprocess$Resolve$lastDeclaration(declarations)));
		return _Utils_ap(
			newDeclarations,
			_Utils_ap(
				withoutParent(initialResult),
				withoutParent(nextResult)));
	});
var $rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles = F2(
	function (styles, declarations) {
		if (!styles.b) {
			return declarations;
		} else {
			switch (styles.a.$) {
				case 'AppendProperty':
					var property = styles.a.a;
					var rest = styles.b;
					return A2(
						$rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles,
						rest,
						A2($rtfeldman$elm_css$Css$Structure$appendProperty, property, declarations));
				case 'ExtendSelector':
					var _v4 = styles.a;
					var selector = _v4.a;
					var nestedStyles = _v4.b;
					var rest = styles.b;
					return A4(
						$rtfeldman$elm_css$Css$Preprocess$Resolve$applyNestedStylesToLast,
						nestedStyles,
						rest,
						$rtfeldman$elm_css$Css$Structure$appendRepeatableToLastSelector(selector),
						declarations);
				case 'NestSnippet':
					var _v5 = styles.a;
					var selectorCombinator = _v5.a;
					var snippets = _v5.b;
					var rest = styles.b;
					var chain = F2(
						function (_v9, _v10) {
							var originalSequence = _v9.a;
							var originalTuples = _v9.b;
							var originalPseudoElement = _v9.c;
							var newSequence = _v10.a;
							var newTuples = _v10.b;
							var newPseudoElement = _v10.c;
							return A3(
								$rtfeldman$elm_css$Css$Structure$Selector,
								originalSequence,
								_Utils_ap(
									originalTuples,
									A2(
										$elm$core$List$cons,
										_Utils_Tuple2(selectorCombinator, newSequence),
										newTuples)),
								$rtfeldman$elm_css$Css$Preprocess$Resolve$oneOf(
									_List_fromArray(
										[newPseudoElement, originalPseudoElement])));
						});
					var expandDeclaration = function (declaration) {
						switch (declaration.$) {
							case 'StyleBlockDeclaration':
								var _v7 = declaration.a;
								var firstSelector = _v7.a;
								var otherSelectors = _v7.b;
								var nestedStyles = _v7.c;
								var newSelectors = A2(
									$elm$core$List$concatMap,
									function (originalSelector) {
										return A2(
											$elm$core$List$map,
											chain(originalSelector),
											A2($elm$core$List$cons, firstSelector, otherSelectors));
									},
									$rtfeldman$elm_css$Css$Preprocess$Resolve$collectSelectors(declarations));
								var newDeclarations = function () {
									if (!newSelectors.b) {
										return _List_Nil;
									} else {
										var first = newSelectors.a;
										var remainder = newSelectors.b;
										return _List_fromArray(
											[
												$rtfeldman$elm_css$Css$Structure$StyleBlockDeclaration(
												A3($rtfeldman$elm_css$Css$Structure$StyleBlock, first, remainder, _List_Nil))
											]);
									}
								}();
								return A2($rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles, nestedStyles, newDeclarations);
							case 'MediaRule':
								var mediaQueries = declaration.a;
								var styleBlocks = declaration.b;
								return A2($rtfeldman$elm_css$Css$Preprocess$Resolve$resolveMediaRule, mediaQueries, styleBlocks);
							case 'SupportsRule':
								var str = declaration.a;
								var otherSnippets = declaration.b;
								return A2($rtfeldman$elm_css$Css$Preprocess$Resolve$resolveSupportsRule, str, otherSnippets);
							case 'DocumentRule':
								var str1 = declaration.a;
								var str2 = declaration.b;
								var str3 = declaration.c;
								var str4 = declaration.d;
								var styleBlock = declaration.e;
								return A2(
									$elm$core$List$map,
									A4($rtfeldman$elm_css$Css$Preprocess$Resolve$toDocumentRule, str1, str2, str3, str4),
									$rtfeldman$elm_css$Css$Preprocess$Resolve$expandStyleBlock(styleBlock));
							case 'PageRule':
								var properties = declaration.a;
								return _List_fromArray(
									[
										$rtfeldman$elm_css$Css$Structure$PageRule(properties)
									]);
							case 'FontFace':
								var properties = declaration.a;
								return _List_fromArray(
									[
										$rtfeldman$elm_css$Css$Structure$FontFace(properties)
									]);
							case 'Viewport':
								var properties = declaration.a;
								return _List_fromArray(
									[
										$rtfeldman$elm_css$Css$Structure$Viewport(properties)
									]);
							case 'CounterStyle':
								var properties = declaration.a;
								return _List_fromArray(
									[
										$rtfeldman$elm_css$Css$Structure$CounterStyle(properties)
									]);
							default:
								var tuples = declaration.a;
								return $rtfeldman$elm_css$Css$Preprocess$Resolve$resolveFontFeatureValues(tuples);
						}
					};
					return $elm$core$List$concat(
						_Utils_ap(
							_List_fromArray(
								[
									A2($rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles, rest, declarations)
								]),
							A2(
								$elm$core$List$map,
								expandDeclaration,
								A2($elm$core$List$concatMap, $rtfeldman$elm_css$Css$Preprocess$unwrapSnippet, snippets))));
				case 'WithPseudoElement':
					var _v11 = styles.a;
					var pseudoElement = _v11.a;
					var nestedStyles = _v11.b;
					var rest = styles.b;
					return A4(
						$rtfeldman$elm_css$Css$Preprocess$Resolve$applyNestedStylesToLast,
						nestedStyles,
						rest,
						$rtfeldman$elm_css$Css$Structure$appendPseudoElementToLastSelector(pseudoElement),
						declarations);
				case 'WithKeyframes':
					var str = styles.a.a;
					var rest = styles.b;
					var name = $rtfeldman$elm_css$Hash$fromString(str);
					var newProperty = $rtfeldman$elm_css$Css$Structure$Property('animation-name:' + name);
					var newDeclarations = A2(
						$rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles,
						rest,
						A2($rtfeldman$elm_css$Css$Structure$appendProperty, newProperty, declarations));
					return A2(
						$elm$core$List$append,
						newDeclarations,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Css$Structure$Keyframes(
								{declaration: str, name: name})
							]));
				case 'WithMedia':
					var _v12 = styles.a;
					var mediaQueries = _v12.a;
					var nestedStyles = _v12.b;
					var rest = styles.b;
					var extraDeclarations = function () {
						var _v13 = $rtfeldman$elm_css$Css$Preprocess$Resolve$collectSelectors(declarations);
						if (!_v13.b) {
							return _List_Nil;
						} else {
							var firstSelector = _v13.a;
							var otherSelectors = _v13.b;
							return A2(
								$elm$core$List$map,
								$rtfeldman$elm_css$Css$Structure$styleBlockToMediaRule(mediaQueries),
								A2(
									$rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles,
									nestedStyles,
									$elm$core$List$singleton(
										$rtfeldman$elm_css$Css$Structure$StyleBlockDeclaration(
											A3($rtfeldman$elm_css$Css$Structure$StyleBlock, firstSelector, otherSelectors, _List_Nil)))));
						}
					}();
					return _Utils_ap(
						A2($rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles, rest, declarations),
						extraDeclarations);
				default:
					var otherStyles = styles.a.a;
					var rest = styles.b;
					return A2(
						$rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles,
						_Utils_ap(otherStyles, rest),
						declarations);
			}
		}
	});
var $rtfeldman$elm_css$Css$Preprocess$Resolve$expandStyleBlock = function (_v2) {
	var firstSelector = _v2.a;
	var otherSelectors = _v2.b;
	var styles = _v2.c;
	return A2(
		$rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles,
		styles,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Css$Structure$StyleBlockDeclaration(
				A3($rtfeldman$elm_css$Css$Structure$StyleBlock, firstSelector, otherSelectors, _List_Nil))
			]));
};
var $rtfeldman$elm_css$Css$Preprocess$Resolve$extract = function (snippetDeclarations) {
	if (!snippetDeclarations.b) {
		return _List_Nil;
	} else {
		var first = snippetDeclarations.a;
		var rest = snippetDeclarations.b;
		return _Utils_ap(
			$rtfeldman$elm_css$Css$Preprocess$Resolve$toDeclarations(first),
			$rtfeldman$elm_css$Css$Preprocess$Resolve$extract(rest));
	}
};
var $rtfeldman$elm_css$Css$Preprocess$Resolve$resolveMediaRule = F2(
	function (mediaQueries, styleBlocks) {
		var handleStyleBlock = function (styleBlock) {
			return A2(
				$elm$core$List$map,
				$rtfeldman$elm_css$Css$Preprocess$Resolve$toMediaRule(mediaQueries),
				$rtfeldman$elm_css$Css$Preprocess$Resolve$expandStyleBlock(styleBlock));
		};
		return A2($elm$core$List$concatMap, handleStyleBlock, styleBlocks);
	});
var $rtfeldman$elm_css$Css$Preprocess$Resolve$resolveSupportsRule = F2(
	function (str, snippets) {
		var declarations = $rtfeldman$elm_css$Css$Preprocess$Resolve$extract(
			A2($elm$core$List$concatMap, $rtfeldman$elm_css$Css$Preprocess$unwrapSnippet, snippets));
		return _List_fromArray(
			[
				A2($rtfeldman$elm_css$Css$Structure$SupportsRule, str, declarations)
			]);
	});
var $rtfeldman$elm_css$Css$Preprocess$Resolve$toDeclarations = function (snippetDeclaration) {
	switch (snippetDeclaration.$) {
		case 'StyleBlockDeclaration':
			var styleBlock = snippetDeclaration.a;
			return $rtfeldman$elm_css$Css$Preprocess$Resolve$expandStyleBlock(styleBlock);
		case 'MediaRule':
			var mediaQueries = snippetDeclaration.a;
			var styleBlocks = snippetDeclaration.b;
			return A2($rtfeldman$elm_css$Css$Preprocess$Resolve$resolveMediaRule, mediaQueries, styleBlocks);
		case 'SupportsRule':
			var str = snippetDeclaration.a;
			var snippets = snippetDeclaration.b;
			return A2($rtfeldman$elm_css$Css$Preprocess$Resolve$resolveSupportsRule, str, snippets);
		case 'DocumentRule':
			var str1 = snippetDeclaration.a;
			var str2 = snippetDeclaration.b;
			var str3 = snippetDeclaration.c;
			var str4 = snippetDeclaration.d;
			var styleBlock = snippetDeclaration.e;
			return A2(
				$elm$core$List$map,
				A4($rtfeldman$elm_css$Css$Preprocess$Resolve$toDocumentRule, str1, str2, str3, str4),
				$rtfeldman$elm_css$Css$Preprocess$Resolve$expandStyleBlock(styleBlock));
		case 'PageRule':
			var properties = snippetDeclaration.a;
			return _List_fromArray(
				[
					$rtfeldman$elm_css$Css$Structure$PageRule(properties)
				]);
		case 'FontFace':
			var properties = snippetDeclaration.a;
			return _List_fromArray(
				[
					$rtfeldman$elm_css$Css$Structure$FontFace(properties)
				]);
		case 'Viewport':
			var properties = snippetDeclaration.a;
			return _List_fromArray(
				[
					$rtfeldman$elm_css$Css$Structure$Viewport(properties)
				]);
		case 'CounterStyle':
			var properties = snippetDeclaration.a;
			return _List_fromArray(
				[
					$rtfeldman$elm_css$Css$Structure$CounterStyle(properties)
				]);
		default:
			var tuples = snippetDeclaration.a;
			return $rtfeldman$elm_css$Css$Preprocess$Resolve$resolveFontFeatureValues(tuples);
	}
};
var $rtfeldman$elm_css$Css$Preprocess$Resolve$toStructure = function (_v0) {
	var charset = _v0.charset;
	var imports = _v0.imports;
	var namespaces = _v0.namespaces;
	var snippets = _v0.snippets;
	var declarations = $rtfeldman$elm_css$Css$Preprocess$Resolve$extract(
		A2($elm$core$List$concatMap, $rtfeldman$elm_css$Css$Preprocess$unwrapSnippet, snippets));
	return {charset: charset, declarations: declarations, imports: imports, namespaces: namespaces};
};
var $rtfeldman$elm_css$Css$Preprocess$Resolve$compile = function (sheet) {
	return $rtfeldman$elm_css$Css$Structure$Output$prettyPrint(
		$rtfeldman$elm_css$Css$Structure$compactStylesheet(
			$rtfeldman$elm_css$Css$Preprocess$Resolve$toStructure(sheet)));
};
var $elm$virtual_dom$VirtualDom$node = function (tag) {
	return _VirtualDom_node(
		_VirtualDom_noScript(tag));
};
var $rtfeldman$elm_css$Css$Preprocess$stylesheet = function (snippets) {
	return {charset: $elm$core$Maybe$Nothing, imports: _List_Nil, namespaces: _List_Nil, snippets: snippets};
};
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $rtfeldman$elm_css$VirtualDom$Styled$Unstyled = function (a) {
	return {$: 'Unstyled', a: a};
};
var $rtfeldman$elm_css$VirtualDom$Styled$unstyledNode = $rtfeldman$elm_css$VirtualDom$Styled$Unstyled;
var $rtfeldman$elm_css$Css$Global$global = function (snippets) {
	return $rtfeldman$elm_css$VirtualDom$Styled$unstyledNode(
		A3(
			$elm$virtual_dom$VirtualDom$node,
			'span',
			_List_fromArray(
				[
					A2($elm$virtual_dom$VirtualDom$attribute, 'style', 'display: none;'),
					A2($elm$virtual_dom$VirtualDom$attribute, 'class', 'elm-css-style-wrapper')
				]),
			$elm$core$List$singleton(
				A3(
					$elm$virtual_dom$VirtualDom$node,
					'style',
					_List_Nil,
					$elm$core$List$singleton(
						$elm$virtual_dom$VirtualDom$text(
							$rtfeldman$elm_css$Css$Preprocess$Resolve$compile(
								$rtfeldman$elm_css$Css$Preprocess$stylesheet(snippets))))))));
};
var $rtfeldman$elm_css$Css$Structure$TypeSelector = function (a) {
	return {$: 'TypeSelector', a: a};
};
var $rtfeldman$elm_css$Css$Global$typeSelector = F2(
	function (selectorStr, styles) {
		var sequence = A2(
			$rtfeldman$elm_css$Css$Structure$TypeSelectorSequence,
			$rtfeldman$elm_css$Css$Structure$TypeSelector(selectorStr),
			_List_Nil);
		var sel = A3($rtfeldman$elm_css$Css$Structure$Selector, sequence, _List_Nil, $elm$core$Maybe$Nothing);
		return $rtfeldman$elm_css$Css$Preprocess$Snippet(
			_List_fromArray(
				[
					$rtfeldman$elm_css$Css$Preprocess$StyleBlockDeclaration(
					A3($rtfeldman$elm_css$Css$Preprocess$StyleBlock, sel, _List_Nil, styles))
				]));
	});
var $rtfeldman$elm_css$Css$Global$html = $rtfeldman$elm_css$Css$Global$typeSelector('html');
var $rtfeldman$elm_css$Css$initial = {alignItems: $rtfeldman$elm_css$Css$Structure$Compatible, all: $rtfeldman$elm_css$Css$Structure$Compatible, backgroundAttachment: $rtfeldman$elm_css$Css$Structure$Compatible, backgroundBlendMode: $rtfeldman$elm_css$Css$Structure$Compatible, backgroundImage: $rtfeldman$elm_css$Css$Structure$Compatible, backgroundOrigin: $rtfeldman$elm_css$Css$Structure$Compatible, backgroundRepeat: $rtfeldman$elm_css$Css$Structure$Compatible, backgroundRepeatShorthand: $rtfeldman$elm_css$Css$Structure$Compatible, borderStyle: $rtfeldman$elm_css$Css$Structure$Compatible, boxSizing: $rtfeldman$elm_css$Css$Structure$Compatible, color: $rtfeldman$elm_css$Css$Structure$Compatible, cursor: $rtfeldman$elm_css$Css$Structure$Compatible, display: $rtfeldman$elm_css$Css$Structure$Compatible, flexBasis: $rtfeldman$elm_css$Css$Structure$Compatible, flexDirection: $rtfeldman$elm_css$Css$Structure$Compatible, flexDirectionOrWrap: $rtfeldman$elm_css$Css$Structure$Compatible, flexWrap: $rtfeldman$elm_css$Css$Structure$Compatible, fontFamily: $rtfeldman$elm_css$Css$Structure$Compatible, fontSize: $rtfeldman$elm_css$Css$Structure$Compatible, fontStyle: $rtfeldman$elm_css$Css$Structure$Compatible, fontVariant: $rtfeldman$elm_css$Css$Structure$Compatible, fontWeight: $rtfeldman$elm_css$Css$Structure$Compatible, intOrAuto: $rtfeldman$elm_css$Css$Structure$Compatible, justifyContent: $rtfeldman$elm_css$Css$Structure$Compatible, keyframes: $rtfeldman$elm_css$Css$Structure$Compatible, length: $rtfeldman$elm_css$Css$Structure$Compatible, lengthOrAuto: $rtfeldman$elm_css$Css$Structure$Compatible, lengthOrAutoOrCoverOrContain: $rtfeldman$elm_css$Css$Structure$Compatible, lengthOrMinMaxDimension: $rtfeldman$elm_css$Css$Structure$Compatible, lengthOrNone: $rtfeldman$elm_css$Css$Structure$Compatible, lengthOrNoneOrMinMaxDimension: $rtfeldman$elm_css$Css$Structure$Compatible, lengthOrNumber: $rtfeldman$elm_css$Css$Structure$Compatible, lengthOrNumberOrAutoOrNoneOrContent: $rtfeldman$elm_css$Css$Structure$Compatible, listStylePosition: $rtfeldman$elm_css$Css$Structure$Compatible, listStyleType: $rtfeldman$elm_css$Css$Structure$Compatible, listStyleTypeOrPositionOrImage: $rtfeldman$elm_css$Css$Structure$Compatible, none: $rtfeldman$elm_css$Css$Structure$Compatible, number: $rtfeldman$elm_css$Css$Structure$Compatible, numericValue: 0, outline: $rtfeldman$elm_css$Css$Structure$Compatible, overflow: $rtfeldman$elm_css$Css$Structure$Compatible, pointerEvents: $rtfeldman$elm_css$Css$Structure$Compatible, tableLayout: $rtfeldman$elm_css$Css$Structure$Compatible, textDecorationLine: $rtfeldman$elm_css$Css$Structure$Compatible, textDecorationStyle: $rtfeldman$elm_css$Css$Structure$Compatible, textIndent: $rtfeldman$elm_css$Css$Structure$Compatible, textRendering: $rtfeldman$elm_css$Css$Structure$Compatible, textTransform: $rtfeldman$elm_css$Css$Structure$Compatible, touchAction: $rtfeldman$elm_css$Css$Structure$Compatible, unitLabel: '', units: $rtfeldman$elm_css$Css$Internal$IncompatibleUnits, value: 'initial', visibility: $rtfeldman$elm_css$Css$Structure$Compatible, whiteSpace: $rtfeldman$elm_css$Css$Structure$Compatible};
var $rtfeldman$elm_css$Css$inherit = _Utils_update(
	$rtfeldman$elm_css$Css$initial,
	{value: 'inherit'});
var $BrianHicks$elm_css_reset$Css$Reset$borderBoxV201408 = $rtfeldman$elm_css$Css$Global$global(
	_List_fromArray(
		[
			$rtfeldman$elm_css$Css$Global$html(
			_List_fromArray(
				[
					$rtfeldman$elm_css$Css$boxSizing($rtfeldman$elm_css$Css$borderBox)
				])),
			$rtfeldman$elm_css$Css$Global$everything(
			_List_fromArray(
				[
					$rtfeldman$elm_css$Css$boxSizing($rtfeldman$elm_css$Css$inherit),
					$rtfeldman$elm_css$Css$before(
					_List_fromArray(
						[
							$rtfeldman$elm_css$Css$boxSizing($rtfeldman$elm_css$Css$inherit)
						])),
					$rtfeldman$elm_css$Css$after(
					_List_fromArray(
						[
							$rtfeldman$elm_css$Css$boxSizing($rtfeldman$elm_css$Css$inherit)
						]))
				]))
		]));
var $rtfeldman$elm_css$Css$borderColor = function (c) {
	return A2($rtfeldman$elm_css$Css$property, 'border-color', c.value);
};
var $rtfeldman$elm_css$Css$borderRadius = $rtfeldman$elm_css$Css$prop1('border-radius');
var $rtfeldman$elm_css$Css$borderTopColor = function (c) {
	return A2($rtfeldman$elm_css$Css$property, 'border-top-color', c.value);
};
var $rtfeldman$elm_css$Css$prop4 = F5(
	function (key, argA, argB, argC, argD) {
		return A2($rtfeldman$elm_css$Css$property, key, argA.value + (' ' + (argB.value + (' ' + (argC.value + (' ' + argD.value))))));
	});
var $rtfeldman$elm_css$Css$boxShadow4 = $rtfeldman$elm_css$Css$prop4('box-shadow');
var $rtfeldman$elm_css$VirtualDom$Styled$Node = F3(
	function (a, b, c) {
		return {$: 'Node', a: a, b: b, c: c};
	});
var $rtfeldman$elm_css$VirtualDom$Styled$node = $rtfeldman$elm_css$VirtualDom$Styled$Node;
var $rtfeldman$elm_css$Html$Styled$node = $rtfeldman$elm_css$VirtualDom$Styled$node;
var $rtfeldman$elm_css$Html$Styled$button = $rtfeldman$elm_css$Html$Styled$node('button');
var $rtfeldman$elm_css$Css$center = $rtfeldman$elm_css$Css$prop1('center');
var $rtfeldman$elm_css$VirtualDom$Styled$Attribute = F3(
	function (a, b, c) {
		return {$: 'Attribute', a: a, b: b, c: c};
	});
var $elm$virtual_dom$VirtualDom$property = F2(
	function (key, value) {
		return A2(
			_VirtualDom_property,
			_VirtualDom_noInnerHtmlOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlJson(value));
	});
var $rtfeldman$elm_css$VirtualDom$Styled$property = F2(
	function (key, value) {
		return A3(
			$rtfeldman$elm_css$VirtualDom$Styled$Attribute,
			A2($elm$virtual_dom$VirtualDom$property, key, value),
			false,
			'');
	});
var $rtfeldman$elm_css$Html$Styled$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			$rtfeldman$elm_css$VirtualDom$Styled$property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $rtfeldman$elm_css$Html$Styled$Attributes$checked = $rtfeldman$elm_css$Html$Styled$Attributes$boolProperty('checked');
var $rtfeldman$elm_css$Html$Styled$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			$rtfeldman$elm_css$VirtualDom$Styled$property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $rtfeldman$elm_css$Html$Styled$Attributes$class = $rtfeldman$elm_css$Html$Styled$Attributes$stringProperty('className');
var $rtfeldman$elm_css$Css$color = function (c) {
	return A2($rtfeldman$elm_css$Css$property, 'color', c.value);
};
var $rtfeldman$elm_css$Css$Structure$ClassSelector = function (a) {
	return {$: 'ClassSelector', a: a};
};
var $rtfeldman$elm_css$VirtualDom$Styled$classnameStandin = '\u0007';
var $rtfeldman$elm_css$VirtualDom$Styled$templateSelector = $rtfeldman$elm_css$Css$Structure$UniversalSelectorSequence(
	_List_fromArray(
		[
			$rtfeldman$elm_css$Css$Structure$ClassSelector($rtfeldman$elm_css$VirtualDom$Styled$classnameStandin)
		]));
var $rtfeldman$elm_css$VirtualDom$Styled$getCssTemplate = function (styles) {
	if (!styles.b) {
		return '';
	} else {
		var otherwise = styles;
		return $rtfeldman$elm_css$Css$Preprocess$Resolve$compile(
			$rtfeldman$elm_css$Css$Preprocess$stylesheet(
				_List_fromArray(
					[
						A2($rtfeldman$elm_css$VirtualDom$Styled$makeSnippet, styles, $rtfeldman$elm_css$VirtualDom$Styled$templateSelector)
					])));
	}
};
var $rtfeldman$elm_css$Html$Styled$Internal$css = function (styles) {
	var cssTemplate = $rtfeldman$elm_css$VirtualDom$Styled$getCssTemplate(styles);
	var classProperty = A2($elm$virtual_dom$VirtualDom$attribute, '', '');
	return A3($rtfeldman$elm_css$VirtualDom$Styled$Attribute, classProperty, true, cssTemplate);
};
var $rtfeldman$elm_css$Html$Styled$Attributes$css = $rtfeldman$elm_css$Html$Styled$Internal$css;
var $author$project$Main$KeeperWantsToAddNewPlayer = {$: 'KeeperWantsToAddNewPlayer'};
var $author$project$Main$KeeperWantsToIgnorePlayer = function (a) {
	return {$: 'KeeperWantsToIgnorePlayer', a: a};
};
var $author$project$Main$KeeperWantsToRedo = {$: 'KeeperWantsToRedo'};
var $author$project$Main$KeeperWantsToShowCustomMatchup = {$: 'KeeperWantsToShowCustomMatchup'};
var $author$project$Main$KeeperWantsToSkipMatch = {$: 'KeeperWantsToSkipMatch'};
var $author$project$Main$KeeperWantsToUndo = {$: 'KeeperWantsToUndo'};
var $author$project$Main$KeeperWantsToUnignorePlayer = function (a) {
	return {$: 'KeeperWantsToUnignorePlayer', a: a};
};
var $author$project$Main$MatchFinished = function (a) {
	return {$: 'MatchFinished', a: a};
};
var $rtfeldman$elm_css$Css$absolute = {position: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'absolute'};
var $rtfeldman$elm_css$Css$display = $rtfeldman$elm_css$Css$prop1('display');
var $rtfeldman$elm_css$Css$fontSize = $rtfeldman$elm_css$Css$prop1('font-size');
var $rtfeldman$elm_css$Css$fontWeight = function (_v0) {
	var value = _v0.value;
	return A2($rtfeldman$elm_css$Css$property, 'font-weight', value);
};
var $rtfeldman$elm_css$Css$withPrecedingHash = function (str) {
	return A2($elm$core$String$startsWith, '#', str) ? str : A2(
		$elm$core$String$cons,
		_Utils_chr('#'),
		str);
};
var $rtfeldman$elm_css$Css$erroneousHex = function (str) {
	return {
		alpha: 1,
		blue: 0,
		color: $rtfeldman$elm_css$Css$Structure$Compatible,
		green: 0,
		red: 0,
		value: $rtfeldman$elm_css$Css$withPrecedingHash(str)
	};
};
var $elm$core$String$foldr = _String_foldr;
var $elm$core$String$toList = function (string) {
	return A3($elm$core$String$foldr, $elm$core$List$cons, _List_Nil, string);
};
var $rtfeldman$elm_hex$Hex$fromStringHelp = F3(
	function (position, chars, accumulated) {
		fromStringHelp:
		while (true) {
			if (!chars.b) {
				return $elm$core$Result$Ok(accumulated);
			} else {
				var _char = chars.a;
				var rest = chars.b;
				switch (_char.valueOf()) {
					case '0':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated;
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '1':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + A2($elm$core$Basics$pow, 16, position);
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '2':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (2 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '3':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (3 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '4':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (4 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '5':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (5 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '6':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (6 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '7':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (7 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '8':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (8 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '9':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (9 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'a':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (10 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'b':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (11 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'c':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (12 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'd':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (13 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'e':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (14 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'f':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (15 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					default:
						var nonHex = _char;
						return $elm$core$Result$Err(
							$elm$core$String$fromChar(nonHex) + ' is not a valid hexadecimal character.');
				}
			}
		}
	});
var $elm$core$Result$map = F2(
	function (func, ra) {
		if (ra.$ === 'Ok') {
			var a = ra.a;
			return $elm$core$Result$Ok(
				func(a));
		} else {
			var e = ra.a;
			return $elm$core$Result$Err(e);
		}
	});
var $rtfeldman$elm_hex$Hex$fromString = function (str) {
	if ($elm$core$String$isEmpty(str)) {
		return $elm$core$Result$Err('Empty strings are not valid hexadecimal strings.');
	} else {
		var result = function () {
			if (A2($elm$core$String$startsWith, '-', str)) {
				var list = A2(
					$elm$core$Maybe$withDefault,
					_List_Nil,
					$elm$core$List$tail(
						$elm$core$String$toList(str)));
				return A2(
					$elm$core$Result$map,
					$elm$core$Basics$negate,
					A3(
						$rtfeldman$elm_hex$Hex$fromStringHelp,
						$elm$core$List$length(list) - 1,
						list,
						0));
			} else {
				return A3(
					$rtfeldman$elm_hex$Hex$fromStringHelp,
					$elm$core$String$length(str) - 1,
					$elm$core$String$toList(str),
					0);
			}
		}();
		var formatError = function (err) {
			return A2(
				$elm$core$String$join,
				' ',
				_List_fromArray(
					['\"' + (str + '\"'), 'is not a valid hexadecimal string because', err]));
		};
		return A2($elm$core$Result$mapError, formatError, result);
	}
};
var $rtfeldman$elm_css$Css$validHex = F5(
	function (str, _v0, _v1, _v2, _v3) {
		var r1 = _v0.a;
		var r2 = _v0.b;
		var g1 = _v1.a;
		var g2 = _v1.b;
		var b1 = _v2.a;
		var b2 = _v2.b;
		var a1 = _v3.a;
		var a2 = _v3.b;
		var toResult = A2(
			$elm$core$Basics$composeR,
			$elm$core$String$fromList,
			A2($elm$core$Basics$composeR, $elm$core$String$toLower, $rtfeldman$elm_hex$Hex$fromString));
		var results = _Utils_Tuple2(
			_Utils_Tuple2(
				toResult(
					_List_fromArray(
						[r1, r2])),
				toResult(
					_List_fromArray(
						[g1, g2]))),
			_Utils_Tuple2(
				toResult(
					_List_fromArray(
						[b1, b2])),
				toResult(
					_List_fromArray(
						[a1, a2]))));
		if ((((results.a.a.$ === 'Ok') && (results.a.b.$ === 'Ok')) && (results.b.a.$ === 'Ok')) && (results.b.b.$ === 'Ok')) {
			var _v5 = results.a;
			var red = _v5.a.a;
			var green = _v5.b.a;
			var _v6 = results.b;
			var blue = _v6.a.a;
			var alpha = _v6.b.a;
			return {
				alpha: alpha / 255,
				blue: blue,
				color: $rtfeldman$elm_css$Css$Structure$Compatible,
				green: green,
				red: red,
				value: $rtfeldman$elm_css$Css$withPrecedingHash(str)
			};
		} else {
			return $rtfeldman$elm_css$Css$erroneousHex(str);
		}
	});
var $rtfeldman$elm_css$Css$hex = function (str) {
	var withoutHash = A2($elm$core$String$startsWith, '#', str) ? A2($elm$core$String$dropLeft, 1, str) : str;
	var _v0 = $elm$core$String$toList(withoutHash);
	_v0$4:
	while (true) {
		if ((_v0.b && _v0.b.b) && _v0.b.b.b) {
			if (!_v0.b.b.b.b) {
				var r = _v0.a;
				var _v1 = _v0.b;
				var g = _v1.a;
				var _v2 = _v1.b;
				var b = _v2.a;
				return A5(
					$rtfeldman$elm_css$Css$validHex,
					str,
					_Utils_Tuple2(r, r),
					_Utils_Tuple2(g, g),
					_Utils_Tuple2(b, b),
					_Utils_Tuple2(
						_Utils_chr('f'),
						_Utils_chr('f')));
			} else {
				if (!_v0.b.b.b.b.b) {
					var r = _v0.a;
					var _v3 = _v0.b;
					var g = _v3.a;
					var _v4 = _v3.b;
					var b = _v4.a;
					var _v5 = _v4.b;
					var a = _v5.a;
					return A5(
						$rtfeldman$elm_css$Css$validHex,
						str,
						_Utils_Tuple2(r, r),
						_Utils_Tuple2(g, g),
						_Utils_Tuple2(b, b),
						_Utils_Tuple2(a, a));
				} else {
					if (_v0.b.b.b.b.b.b) {
						if (!_v0.b.b.b.b.b.b.b) {
							var r1 = _v0.a;
							var _v6 = _v0.b;
							var r2 = _v6.a;
							var _v7 = _v6.b;
							var g1 = _v7.a;
							var _v8 = _v7.b;
							var g2 = _v8.a;
							var _v9 = _v8.b;
							var b1 = _v9.a;
							var _v10 = _v9.b;
							var b2 = _v10.a;
							return A5(
								$rtfeldman$elm_css$Css$validHex,
								str,
								_Utils_Tuple2(r1, r2),
								_Utils_Tuple2(g1, g2),
								_Utils_Tuple2(b1, b2),
								_Utils_Tuple2(
									_Utils_chr('f'),
									_Utils_chr('f')));
						} else {
							if (_v0.b.b.b.b.b.b.b.b && (!_v0.b.b.b.b.b.b.b.b.b)) {
								var r1 = _v0.a;
								var _v11 = _v0.b;
								var r2 = _v11.a;
								var _v12 = _v11.b;
								var g1 = _v12.a;
								var _v13 = _v12.b;
								var g2 = _v13.a;
								var _v14 = _v13.b;
								var b1 = _v14.a;
								var _v15 = _v14.b;
								var b2 = _v15.a;
								var _v16 = _v15.b;
								var a1 = _v16.a;
								var _v17 = _v16.b;
								var a2 = _v17.a;
								return A5(
									$rtfeldman$elm_css$Css$validHex,
									str,
									_Utils_Tuple2(r1, r2),
									_Utils_Tuple2(g1, g2),
									_Utils_Tuple2(b1, b2),
									_Utils_Tuple2(a1, a2));
							} else {
								break _v0$4;
							}
						}
					} else {
						break _v0$4;
					}
				}
			}
		} else {
			break _v0$4;
		}
	}
	return $rtfeldman$elm_css$Css$erroneousHex(str);
};
var $rtfeldman$elm_css$Css$inlineBlock = {display: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'inline-block'};
var $rtfeldman$elm_css$Css$UnitlessInteger = {$: 'UnitlessInteger'};
var $rtfeldman$elm_css$Css$int = function (val) {
	return {
		fontWeight: $rtfeldman$elm_css$Css$Structure$Compatible,
		intOrAuto: $rtfeldman$elm_css$Css$Structure$Compatible,
		lengthOrNumber: $rtfeldman$elm_css$Css$Structure$Compatible,
		lengthOrNumberOrAutoOrNoneOrContent: $rtfeldman$elm_css$Css$Structure$Compatible,
		number: $rtfeldman$elm_css$Css$Structure$Compatible,
		numberOrInfinite: $rtfeldman$elm_css$Css$Structure$Compatible,
		numericValue: val,
		unitLabel: '',
		units: $rtfeldman$elm_css$Css$UnitlessInteger,
		value: $elm$core$String$fromInt(val)
	};
};
var $rtfeldman$elm_css$Css$letterSpacing = $rtfeldman$elm_css$Css$prop1('letter-spacing');
var $rtfeldman$elm_css$Css$prop2 = F3(
	function (key, argA, argB) {
		return A2($rtfeldman$elm_css$Css$property, key, argA.value + (' ' + argB.value));
	});
var $rtfeldman$elm_css$Css$padding2 = $rtfeldman$elm_css$Css$prop2('padding');
var $rtfeldman$elm_css$Css$PxUnits = {$: 'PxUnits'};
var $rtfeldman$elm_css$Css$px = A2($rtfeldman$elm_css$Css$Internal$lengthConverter, $rtfeldman$elm_css$Css$PxUnits, 'px');
var $rtfeldman$elm_css$Html$Styled$span = $rtfeldman$elm_css$Html$Styled$node('span');
var $rtfeldman$elm_css$VirtualDom$Styled$text = function (str) {
	return $rtfeldman$elm_css$VirtualDom$Styled$Unstyled(
		$elm$virtual_dom$VirtualDom$text(str));
};
var $rtfeldman$elm_css$Html$Styled$text = $rtfeldman$elm_css$VirtualDom$Styled$text;
var $author$project$Main$badge = F3(
	function (label, isOn, colorOn) {
		return A2(
			$rtfeldman$elm_css$Html$Styled$span,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$css(
					_List_fromArray(
						[
							$rtfeldman$elm_css$Css$display($rtfeldman$elm_css$Css$inlineBlock),
							A2(
							$rtfeldman$elm_css$Css$padding2,
							$rtfeldman$elm_css$Css$px(2),
							$rtfeldman$elm_css$Css$px(8)),
							$rtfeldman$elm_css$Css$borderRadius(
							$rtfeldman$elm_css$Css$px(9999)),
							isOn ? $rtfeldman$elm_css$Css$backgroundColor(colorOn) : $rtfeldman$elm_css$Css$backgroundColor(
							$rtfeldman$elm_css$Css$hex('E5E7EB')),
							isOn ? $rtfeldman$elm_css$Css$color(
							$rtfeldman$elm_css$Css$hex('FFFFFF')) : $rtfeldman$elm_css$Css$color(
							$rtfeldman$elm_css$Css$hex('6B7280')),
							$rtfeldman$elm_css$Css$fontSize(
							$rtfeldman$elm_css$Css$px(12)),
							$rtfeldman$elm_css$Css$fontWeight(
							$rtfeldman$elm_css$Css$int(700)),
							$rtfeldman$elm_css$Css$letterSpacing(
							$rtfeldman$elm_css$Css$px(0.5))
						]))
				]),
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$text(label)
				]));
	});
var $rtfeldman$elm_css$Css$displayFlex = A2($rtfeldman$elm_css$Css$property, 'display', 'flex');
var $rtfeldman$elm_css$Html$Styled$div = $rtfeldman$elm_css$Html$Styled$node('div');
var $rtfeldman$elm_css$Css$justifyContent = function (fn) {
	return A3(
		$rtfeldman$elm_css$Css$Internal$getOverloadedProperty,
		'justifyContent',
		'justify-content',
		fn($rtfeldman$elm_css$Css$Internal$lengthForOverloadedProperty));
};
var $rtfeldman$elm_css$Css$marginRight = $rtfeldman$elm_css$Css$prop1('margin-right');
var $author$project$Main$availabilityBadges = function (player) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$div,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$Attributes$css(
				_List_fromArray(
					[
						$rtfeldman$elm_css$Css$displayFlex,
						$rtfeldman$elm_css$Css$justifyContent($rtfeldman$elm_css$Css$center)
					]))
			]),
		_List_fromArray(
			[
				A2(
				$rtfeldman$elm_css$Html$Styled$span,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Attributes$css(
						_List_fromArray(
							[
								$rtfeldman$elm_css$Css$marginRight(
								$rtfeldman$elm_css$Css$px(6))
							]))
					]),
				_List_fromArray(
					[
						A3(
						$author$project$Main$badge,
						'AM',
						$author$project$Player$playsAM(player),
						$rtfeldman$elm_css$Css$hex('F59E0B'))
					])),
				A3(
				$author$project$Main$badge,
				'PM',
				$author$project$Player$playsPM(player),
				$rtfeldman$elm_css$Css$hex('8B5CF6'))
			]));
};
var $rtfeldman$elm_css$Css$fontStyle = $rtfeldman$elm_css$Css$prop1('font-style');
var $rtfeldman$elm_css$Html$Styled$h2 = $rtfeldman$elm_css$Html$Styled$node('h2');
var $rtfeldman$elm_css$Css$italic = {fontStyle: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'italic'};
var $rtfeldman$elm_css$Css$marginBottom = $rtfeldman$elm_css$Css$prop1('margin-bottom');
var $rtfeldman$elm_css$Css$maxWidth = $rtfeldman$elm_css$Css$prop1('max-width');
var $rtfeldman$elm_css$Css$stringsToValue = function (list) {
	return $elm$core$List$isEmpty(list) ? {value: 'none'} : {
		value: A2($elm$core$String$join, ', ', list)
	};
};
var $rtfeldman$elm_css$Css$fontFamilies = A2(
	$elm$core$Basics$composeL,
	$rtfeldman$elm_css$Css$prop1('font-family'),
	$rtfeldman$elm_css$Css$stringsToValue);
var $author$project$Main$modernSansSerif = $rtfeldman$elm_css$Css$fontFamilies(
	_List_fromArray(
		['system-ui', '-apple-system', 'BlinkMacSystemFont', '\'Segoe UI\'', '\'Roboto\'', '\'Inter\'', '\'Helvetica Neue\'', 'Arial', 'sans-serif']));
var $rtfeldman$elm_css$Css$PercentageUnits = {$: 'PercentageUnits'};
var $rtfeldman$elm_css$Css$pct = A2($rtfeldman$elm_css$Css$Internal$lengthConverter, $rtfeldman$elm_css$Css$PercentageUnits, '%');
var $rtfeldman$elm_css$Css$textAlign = function (fn) {
	return A3(
		$rtfeldman$elm_css$Css$Internal$getOverloadedProperty,
		'textAlign',
		'text-align',
		fn($rtfeldman$elm_css$Css$Internal$lengthForOverloadedProperty));
};
var $rtfeldman$elm_css$Css$textTransform = $rtfeldman$elm_css$Css$prop1('text-transform');
var $rtfeldman$elm_css$Css$uppercase = {textTransform: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'uppercase'};
var $rtfeldman$elm_css$Css$width = $rtfeldman$elm_css$Css$prop1('width');
var $author$project$Main$activePlayer = function (player) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$div,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$Attributes$css(
				_List_fromArray(
					[
						$rtfeldman$elm_css$Css$width(
						$rtfeldman$elm_css$Css$pct(40)),
						$rtfeldman$elm_css$Css$maxWidth(
						$rtfeldman$elm_css$Css$pct(45)),
						$rtfeldman$elm_css$Css$textAlign($rtfeldman$elm_css$Css$center),
						$author$project$Main$modernSansSerif
					]))
			]),
		_List_fromArray(
			[
				A2(
				$rtfeldman$elm_css$Html$Styled$h2,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Attributes$css(
						_List_fromArray(
							[
								$rtfeldman$elm_css$Css$fontSize(
								$rtfeldman$elm_css$Css$px(26)),
								$rtfeldman$elm_css$Css$marginBottom(
								$rtfeldman$elm_css$Css$px(6)),
								$rtfeldman$elm_css$Css$textTransform($rtfeldman$elm_css$Css$uppercase),
								$rtfeldman$elm_css$Css$fontWeight(
								$rtfeldman$elm_css$Css$int(700)),
								$rtfeldman$elm_css$Css$fontStyle($rtfeldman$elm_css$Css$italic)
							]))
					]),
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$text(
						$author$project$Player$name(player))
					])),
				$author$project$Main$availabilityBadges(player)
			]));
};
var $rtfeldman$elm_css$Html$Styled$h3 = $rtfeldman$elm_css$Html$Styled$node('h3');
var $rtfeldman$elm_css$Css$cursor = $rtfeldman$elm_css$Css$prop1('cursor');
var $rtfeldman$elm_css$Html$Styled$Attributes$disabled = $rtfeldman$elm_css$Html$Styled$Attributes$boolProperty('disabled');
var $rtfeldman$elm_css$Css$margin2 = $rtfeldman$elm_css$Css$prop2('margin');
var $rtfeldman$elm_css$Css$minWidth = $rtfeldman$elm_css$Css$prop1('min-width');
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 'Normal', a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $rtfeldman$elm_css$VirtualDom$Styled$on = F2(
	function (eventName, handler) {
		return A3(
			$rtfeldman$elm_css$VirtualDom$Styled$Attribute,
			A2($elm$virtual_dom$VirtualDom$on, eventName, handler),
			false,
			'');
	});
var $rtfeldman$elm_css$Html$Styled$Events$on = F2(
	function (event, decoder) {
		return A2(
			$rtfeldman$elm_css$VirtualDom$Styled$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $rtfeldman$elm_css$Html$Styled$Events$onClick = function (msg) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $rtfeldman$elm_css$Css$paddingBottom = $rtfeldman$elm_css$Css$prop1('padding-bottom');
var $rtfeldman$elm_css$Css$paddingLeft = $rtfeldman$elm_css$Css$prop1('padding-left');
var $rtfeldman$elm_css$Css$paddingRight = $rtfeldman$elm_css$Css$prop1('padding-right');
var $rtfeldman$elm_css$Css$paddingTop = $rtfeldman$elm_css$Css$prop1('padding-top');
var $rtfeldman$elm_css$Css$pointer = {cursor: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'pointer'};
var $rtfeldman$elm_css$Css$zero = {length: $rtfeldman$elm_css$Css$Structure$Compatible, lengthOrAuto: $rtfeldman$elm_css$Css$Structure$Compatible, lengthOrAutoOrCoverOrContain: $rtfeldman$elm_css$Css$Structure$Compatible, lengthOrMinMaxDimension: $rtfeldman$elm_css$Css$Structure$Compatible, lengthOrNone: $rtfeldman$elm_css$Css$Structure$Compatible, lengthOrNoneOrMinMaxDimension: $rtfeldman$elm_css$Css$Structure$Compatible, lengthOrNumber: $rtfeldman$elm_css$Css$Structure$Compatible, number: $rtfeldman$elm_css$Css$Structure$Compatible, numericValue: 0, outline: $rtfeldman$elm_css$Css$Structure$Compatible, unitLabel: '', units: $rtfeldman$elm_css$Css$UnitlessInteger, value: '0'};
var $author$project$Main$zzzIgnoreButtonTiny = function (maybeMsg) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$button,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$Attributes$css(
				_List_fromArray(
					[
						$rtfeldman$elm_css$Css$paddingTop(
						$rtfeldman$elm_css$Css$px(1)),
						$rtfeldman$elm_css$Css$paddingBottom(
						$rtfeldman$elm_css$Css$px(2)),
						$rtfeldman$elm_css$Css$paddingLeft(
						$rtfeldman$elm_css$Css$px(6)),
						$rtfeldman$elm_css$Css$paddingRight(
						$rtfeldman$elm_css$Css$px(6)),
						A2(
						$rtfeldman$elm_css$Css$margin2,
						$rtfeldman$elm_css$Css$zero,
						$rtfeldman$elm_css$Css$px(4)),
						$rtfeldman$elm_css$Css$minWidth(
						$rtfeldman$elm_css$Css$px(24)),
						$rtfeldman$elm_css$Css$backgroundColor(
						$rtfeldman$elm_css$Css$hex('6B7280')),
						$rtfeldman$elm_css$Css$border($rtfeldman$elm_css$Css$zero),
						$rtfeldman$elm_css$Css$borderRadius(
						$rtfeldman$elm_css$Css$px(9999)),
						$rtfeldman$elm_css$Css$cursor($rtfeldman$elm_css$Css$pointer),
						$rtfeldman$elm_css$Css$fontSize(
						$rtfeldman$elm_css$Css$px(9)),
						$rtfeldman$elm_css$Css$fontWeight(
						$rtfeldman$elm_css$Css$int(700)),
						$rtfeldman$elm_css$Css$color(
						$rtfeldman$elm_css$Css$hex('FFF')),
						$author$project$Main$modernSansSerif
					])),
				function () {
				if (maybeMsg.$ === 'Just') {
					var m = maybeMsg.a;
					return $rtfeldman$elm_css$Html$Styled$Events$onClick(m);
				} else {
					return $rtfeldman$elm_css$Html$Styled$Attributes$disabled(true);
				}
			}()
			]),
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$text('Zzz')
			]));
};
var $rtfeldman$elm_css$Css$lineThrough = {textDecorationLine: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'line-through'};
var $rtfeldman$elm_css$Css$textDecoration = $rtfeldman$elm_css$Css$prop1('text-decoration');
var $author$project$Main$zzzUnignoreButtonTiny = function (maybeMsg) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$button,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$Attributes$css(
				_List_fromArray(
					[
						$rtfeldman$elm_css$Css$paddingTop(
						$rtfeldman$elm_css$Css$px(1)),
						$rtfeldman$elm_css$Css$paddingBottom(
						$rtfeldman$elm_css$Css$px(2)),
						$rtfeldman$elm_css$Css$paddingLeft(
						$rtfeldman$elm_css$Css$px(6)),
						$rtfeldman$elm_css$Css$paddingRight(
						$rtfeldman$elm_css$Css$px(6)),
						A2(
						$rtfeldman$elm_css$Css$margin2,
						$rtfeldman$elm_css$Css$zero,
						$rtfeldman$elm_css$Css$px(4)),
						$rtfeldman$elm_css$Css$minWidth(
						$rtfeldman$elm_css$Css$px(24)),
						$rtfeldman$elm_css$Css$backgroundColor(
						$rtfeldman$elm_css$Css$hex('374151')),
						$rtfeldman$elm_css$Css$border($rtfeldman$elm_css$Css$zero),
						$rtfeldman$elm_css$Css$borderRadius(
						$rtfeldman$elm_css$Css$px(9999)),
						$rtfeldman$elm_css$Css$cursor($rtfeldman$elm_css$Css$pointer),
						$rtfeldman$elm_css$Css$fontSize(
						$rtfeldman$elm_css$Css$px(9)),
						$rtfeldman$elm_css$Css$fontWeight(
						$rtfeldman$elm_css$Css$int(700)),
						$rtfeldman$elm_css$Css$color(
						$rtfeldman$elm_css$Css$hex('FFF')),
						$rtfeldman$elm_css$Css$textDecoration($rtfeldman$elm_css$Css$lineThrough),
						$author$project$Main$modernSansSerif
					])),
				function () {
				if (maybeMsg.$ === 'Just') {
					var m = maybeMsg.a;
					return $rtfeldman$elm_css$Html$Styled$Events$onClick(m);
				} else {
					return $rtfeldman$elm_css$Html$Styled$Attributes$disabled(true);
				}
			}()
			]),
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$text('Zzz')
			]));
};
var $author$project$Main$activePlayerCompactWithIgnore = F2(
	function (player, model) {
		return A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$css(
					_List_fromArray(
						[$author$project$Main$modernSansSerif]))
				]),
			_List_fromArray(
				[
					A2(
					$rtfeldman$elm_css$Html$Styled$h3,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$css(
							_List_fromArray(
								[
									$rtfeldman$elm_css$Css$fontSize(
									$rtfeldman$elm_css$Css$px(20)),
									$rtfeldman$elm_css$Css$marginBottom(
									$rtfeldman$elm_css$Css$px(4)),
									$rtfeldman$elm_css$Css$textTransform($rtfeldman$elm_css$Css$uppercase),
									$rtfeldman$elm_css$Css$fontWeight(
									$rtfeldman$elm_css$Css$int(700)),
									$rtfeldman$elm_css$Css$fontStyle($rtfeldman$elm_css$Css$italic)
								]))
						]),
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text(
							$author$project$Player$name(player))
						])),
					A2(
					$rtfeldman$elm_css$Html$Styled$div,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$css(
							_List_fromArray(
								[
									$rtfeldman$elm_css$Css$displayFlex,
									$rtfeldman$elm_css$Css$justifyContent($rtfeldman$elm_css$Css$center),
									$rtfeldman$elm_css$Css$alignItems($rtfeldman$elm_css$Css$center)
								]))
						]),
					_List_fromArray(
						[
							A2(
							$rtfeldman$elm_css$Html$Styled$span,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$css(
									_List_fromArray(
										[
											$rtfeldman$elm_css$Css$marginRight(
											$rtfeldman$elm_css$Css$px(6))
										]))
								]),
							_List_fromArray(
								[
									A3(
									$author$project$Main$badge,
									'AM',
									$author$project$Player$playsAM(player),
									$rtfeldman$elm_css$Css$hex('F59E0B'))
								])),
							A2(
							$rtfeldman$elm_css$Html$Styled$span,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$css(
									_List_fromArray(
										[
											$rtfeldman$elm_css$Css$marginRight(
											$rtfeldman$elm_css$Css$px(6))
										]))
								]),
							_List_fromArray(
								[
									A3(
									$author$project$Main$badge,
									'PM',
									$author$project$Player$playsPM(player),
									$rtfeldman$elm_css$Css$hex('8B5CF6'))
								])),
							A2($author$project$Main$isPlayerLocallyIgnored, player, model) ? $author$project$Main$zzzUnignoreButtonTiny(
							$elm$core$Maybe$Just(
								$author$project$Main$KeeperWantsToUnignorePlayer(player))) : $author$project$Main$zzzIgnoreButtonTiny(
							$elm$core$Maybe$Just(
								$author$project$Main$KeeperWantsToIgnorePlayer(player)))
						]))
				]));
	});
var $rtfeldman$elm_css$Css$prop6 = F7(
	function (key, argA, argB, argC, argD, argE, argF) {
		return A2($rtfeldman$elm_css$Css$property, key, argA.value + (' ' + (argB.value + (' ' + (argC.value + (' ' + (argD.value + (' ' + (argE.value + (' ' + argF.value))))))))));
	});
var $rtfeldman$elm_css$Css$boxShadow6 = $rtfeldman$elm_css$Css$prop6('box-shadow');
var $rtfeldman$elm_css$Css$inset = {borderStyle: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'inset'};
var $rtfeldman$elm_css$Css$cssFunction = F2(
	function (funcName, args) {
		return funcName + ('(' + (A2($elm$core$String$join, ',', args) + ')'));
	});
var $rtfeldman$elm_css$Css$rgba = F4(
	function (r, g, b, alpha) {
		return {
			alpha: alpha,
			blue: b,
			color: $rtfeldman$elm_css$Css$Structure$Compatible,
			green: g,
			red: r,
			value: A2(
				$rtfeldman$elm_css$Css$cssFunction,
				'rgba',
				_Utils_ap(
					A2(
						$elm$core$List$map,
						$elm$core$String$fromInt,
						_List_fromArray(
							[r, g, b])),
					_List_fromArray(
						[
							$elm$core$String$fromFloat(alpha)
						])))
		};
	});
var $author$project$Main$button = F3(
	function (baseColor, label, maybeMsg) {
		return A2(
			$rtfeldman$elm_css$Html$Styled$button,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$css(
					_List_fromArray(
						[
							$rtfeldman$elm_css$Css$paddingTop(
							$rtfeldman$elm_css$Css$px(6)),
							$rtfeldman$elm_css$Css$paddingBottom(
							$rtfeldman$elm_css$Css$px(10)),
							$rtfeldman$elm_css$Css$paddingLeft(
							$rtfeldman$elm_css$Css$px(15)),
							$rtfeldman$elm_css$Css$paddingRight(
							$rtfeldman$elm_css$Css$px(15)),
							A2(
							$rtfeldman$elm_css$Css$margin2,
							$rtfeldman$elm_css$Css$zero,
							$rtfeldman$elm_css$Css$px(10)),
							$rtfeldman$elm_css$Css$minWidth(
							$rtfeldman$elm_css$Css$px(100)),
							function () {
							if (maybeMsg.$ === 'Just') {
								return $rtfeldman$elm_css$Css$backgroundColor(baseColor);
							} else {
								return $rtfeldman$elm_css$Css$backgroundColor(
									$rtfeldman$elm_css$Css$hex('DDD'));
							}
						}(),
							$rtfeldman$elm_css$Css$border($rtfeldman$elm_css$Css$zero),
							$rtfeldman$elm_css$Css$borderRadius(
							$rtfeldman$elm_css$Css$px(4)),
							A6(
							$rtfeldman$elm_css$Css$boxShadow6,
							$rtfeldman$elm_css$Css$inset,
							$rtfeldman$elm_css$Css$zero,
							$rtfeldman$elm_css$Css$px(-4),
							$rtfeldman$elm_css$Css$zero,
							$rtfeldman$elm_css$Css$zero,
							A4($rtfeldman$elm_css$Css$rgba, 0, 0, 0, 0.1)),
							$rtfeldman$elm_css$Css$cursor($rtfeldman$elm_css$Css$pointer),
							$rtfeldman$elm_css$Css$fontSize(
							$rtfeldman$elm_css$Css$px(14)),
							$rtfeldman$elm_css$Css$fontWeight(
							$rtfeldman$elm_css$Css$int(600)),
							$rtfeldman$elm_css$Css$color(
							$rtfeldman$elm_css$Css$hex('FFF')),
							$author$project$Main$modernSansSerif
						])),
					function () {
					if (maybeMsg.$ === 'Just') {
						var m = maybeMsg.a;
						return $rtfeldman$elm_css$Html$Styled$Events$onClick(m);
					} else {
						return $rtfeldman$elm_css$Html$Styled$Attributes$disabled(true);
					}
				}()
				]),
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$text(label)
				]));
	});
var $author$project$Main$blackButton = $author$project$Main$button(
	$rtfeldman$elm_css$Css$hex('1F2937'));
var $author$project$Main$blueButton = $author$project$Main$button(
	$rtfeldman$elm_css$Css$hex('3B82F6'));
var $author$project$Main$buttonLarge = F3(
	function (baseColor, label, maybeMsg) {
		return A2(
			$rtfeldman$elm_css$Html$Styled$button,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$css(
					_List_fromArray(
						[
							$rtfeldman$elm_css$Css$paddingTop(
							$rtfeldman$elm_css$Css$px(12)),
							$rtfeldman$elm_css$Css$paddingBottom(
							$rtfeldman$elm_css$Css$px(16)),
							$rtfeldman$elm_css$Css$paddingLeft(
							$rtfeldman$elm_css$Css$px(18)),
							$rtfeldman$elm_css$Css$paddingRight(
							$rtfeldman$elm_css$Css$px(18)),
							A2(
							$rtfeldman$elm_css$Css$margin2,
							$rtfeldman$elm_css$Css$zero,
							$rtfeldman$elm_css$Css$px(10)),
							$rtfeldman$elm_css$Css$minWidth(
							$rtfeldman$elm_css$Css$px(140)),
							function () {
							if (maybeMsg.$ === 'Just') {
								return $rtfeldman$elm_css$Css$backgroundColor(baseColor);
							} else {
								return $rtfeldman$elm_css$Css$backgroundColor(
									$rtfeldman$elm_css$Css$hex('DDD'));
							}
						}(),
							$rtfeldman$elm_css$Css$border($rtfeldman$elm_css$Css$zero),
							$rtfeldman$elm_css$Css$borderRadius(
							$rtfeldman$elm_css$Css$px(8)),
							A6(
							$rtfeldman$elm_css$Css$boxShadow6,
							$rtfeldman$elm_css$Css$inset,
							$rtfeldman$elm_css$Css$zero,
							$rtfeldman$elm_css$Css$px(-4),
							$rtfeldman$elm_css$Css$zero,
							$rtfeldman$elm_css$Css$zero,
							A4($rtfeldman$elm_css$Css$rgba, 0, 0, 0, 0.1)),
							$rtfeldman$elm_css$Css$cursor($rtfeldman$elm_css$Css$pointer),
							$rtfeldman$elm_css$Css$fontSize(
							$rtfeldman$elm_css$Css$px(18)),
							$rtfeldman$elm_css$Css$fontWeight(
							$rtfeldman$elm_css$Css$int(700)),
							$rtfeldman$elm_css$Css$color(
							$rtfeldman$elm_css$Css$hex('FFF')),
							$author$project$Main$modernSansSerif
						])),
					function () {
					if (maybeMsg.$ === 'Just') {
						var m = maybeMsg.a;
						return $rtfeldman$elm_css$Html$Styled$Events$onClick(m);
					} else {
						return $rtfeldman$elm_css$Html$Styled$Attributes$disabled(true);
					}
				}()
				]),
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$text(label)
				]));
	});
var $author$project$Main$blueButtonLarge = $author$project$Main$buttonLarge(
	$rtfeldman$elm_css$Css$hex('3B82F6'));
var $author$project$Main$buttonCompact = F3(
	function (baseColor, label, maybeMsg) {
		return A2(
			$rtfeldman$elm_css$Html$Styled$button,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$css(
					_List_fromArray(
						[
							$rtfeldman$elm_css$Css$paddingTop(
							$rtfeldman$elm_css$Css$px(10)),
							$rtfeldman$elm_css$Css$paddingBottom(
							$rtfeldman$elm_css$Css$px(12)),
							$rtfeldman$elm_css$Css$paddingLeft(
							$rtfeldman$elm_css$Css$px(12)),
							$rtfeldman$elm_css$Css$paddingRight(
							$rtfeldman$elm_css$Css$px(12)),
							A2(
							$rtfeldman$elm_css$Css$margin2,
							$rtfeldman$elm_css$Css$zero,
							$rtfeldman$elm_css$Css$px(4)),
							$rtfeldman$elm_css$Css$minWidth(
							$rtfeldman$elm_css$Css$px(100)),
							function () {
							if (maybeMsg.$ === 'Just') {
								return $rtfeldman$elm_css$Css$backgroundColor(baseColor);
							} else {
								return $rtfeldman$elm_css$Css$backgroundColor(
									$rtfeldman$elm_css$Css$hex('DDD'));
							}
						}(),
							$rtfeldman$elm_css$Css$border($rtfeldman$elm_css$Css$zero),
							$rtfeldman$elm_css$Css$borderRadius(
							$rtfeldman$elm_css$Css$px(8)),
							A6(
							$rtfeldman$elm_css$Css$boxShadow6,
							$rtfeldman$elm_css$Css$inset,
							$rtfeldman$elm_css$Css$zero,
							$rtfeldman$elm_css$Css$px(-4),
							$rtfeldman$elm_css$Css$zero,
							$rtfeldman$elm_css$Css$zero,
							A4($rtfeldman$elm_css$Css$rgba, 0, 0, 0, 0.1)),
							$rtfeldman$elm_css$Css$cursor($rtfeldman$elm_css$Css$pointer),
							$rtfeldman$elm_css$Css$fontSize(
							$rtfeldman$elm_css$Css$px(16)),
							$rtfeldman$elm_css$Css$fontWeight(
							$rtfeldman$elm_css$Css$int(700)),
							$rtfeldman$elm_css$Css$color(
							$rtfeldman$elm_css$Css$hex('FFF')),
							$author$project$Main$modernSansSerif
						])),
					function () {
					if (maybeMsg.$ === 'Just') {
						var m = maybeMsg.a;
						return $rtfeldman$elm_css$Html$Styled$Events$onClick(m);
					} else {
						return $rtfeldman$elm_css$Html$Styled$Attributes$disabled(true);
					}
				}()
				]),
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$text(label)
				]));
	});
var $author$project$Main$KeeperSelectedPlayerA = function (a) {
	return {$: 'KeeperSelectedPlayerA', a: a};
};
var $author$project$Main$KeeperSelectedPlayerB = function (a) {
	return {$: 'KeeperSelectedPlayerB', a: a};
};
var $author$project$Main$KeeperUpdatedPlayerASearch = function (a) {
	return {$: 'KeeperUpdatedPlayerASearch', a: a};
};
var $author$project$Main$KeeperUpdatedPlayerBSearch = function (a) {
	return {$: 'KeeperUpdatedPlayerBSearch', a: a};
};
var $author$project$Main$KeeperWantsToHideCustomMatchup = {$: 'KeeperWantsToHideCustomMatchup'};
var $author$project$Main$KeeperWantsToStartCustomMatch = {$: 'KeeperWantsToStartCustomMatch'};
var $rtfeldman$elm_css$Css$prop5 = F6(
	function (key, argA, argB, argC, argD, argE) {
		return A2($rtfeldman$elm_css$Css$property, key, argA.value + (' ' + (argB.value + (' ' + (argC.value + (' ' + (argD.value + (' ' + argE.value))))))));
	});
var $rtfeldman$elm_css$Css$boxShadow5 = $rtfeldman$elm_css$Css$prop5('box-shadow');
var $rtfeldman$elm_css$Css$Preprocess$ExtendSelector = F2(
	function (a, b) {
		return {$: 'ExtendSelector', a: a, b: b};
	});
var $rtfeldman$elm_css$Css$Structure$PseudoClassSelector = function (a) {
	return {$: 'PseudoClassSelector', a: a};
};
var $rtfeldman$elm_css$Css$pseudoClass = function (_class) {
	return $rtfeldman$elm_css$Css$Preprocess$ExtendSelector(
		$rtfeldman$elm_css$Css$Structure$PseudoClassSelector(_class));
};
var $rtfeldman$elm_css$Css$focus = $rtfeldman$elm_css$Css$pseudoClass('focus');
var $rtfeldman$elm_css$Css$hover = $rtfeldman$elm_css$Css$pseudoClass('hover');
var $rtfeldman$elm_css$Html$Styled$input = $rtfeldman$elm_css$Html$Styled$node('input');
var $rtfeldman$elm_css$Html$Styled$label = $rtfeldman$elm_css$Html$Styled$node('label');
var $rtfeldman$elm_css$Css$left = $rtfeldman$elm_css$Css$prop1('left');
var $rtfeldman$elm_css$Css$margin = $rtfeldman$elm_css$Css$prop1('margin');
var $rtfeldman$elm_css$Css$maxHeight = $rtfeldman$elm_css$Css$prop1('max-height');
var $rtfeldman$elm_css$Css$none = {backgroundImage: $rtfeldman$elm_css$Css$Structure$Compatible, blockAxisOverflow: $rtfeldman$elm_css$Css$Structure$Compatible, borderStyle: $rtfeldman$elm_css$Css$Structure$Compatible, cursor: $rtfeldman$elm_css$Css$Structure$Compatible, display: $rtfeldman$elm_css$Css$Structure$Compatible, hoverCapability: $rtfeldman$elm_css$Css$Structure$Compatible, inlineAxisOverflow: $rtfeldman$elm_css$Css$Structure$Compatible, keyframes: $rtfeldman$elm_css$Css$Structure$Compatible, lengthOrNone: $rtfeldman$elm_css$Css$Structure$Compatible, lengthOrNoneOrMinMaxDimension: $rtfeldman$elm_css$Css$Structure$Compatible, lengthOrNumberOrAutoOrNoneOrContent: $rtfeldman$elm_css$Css$Structure$Compatible, listStyleType: $rtfeldman$elm_css$Css$Structure$Compatible, listStyleTypeOrPositionOrImage: $rtfeldman$elm_css$Css$Structure$Compatible, none: $rtfeldman$elm_css$Css$Structure$Compatible, outline: $rtfeldman$elm_css$Css$Structure$Compatible, pointerDevice: $rtfeldman$elm_css$Css$Structure$Compatible, pointerEvents: $rtfeldman$elm_css$Css$Structure$Compatible, resize: $rtfeldman$elm_css$Css$Structure$Compatible, scriptingSupport: $rtfeldman$elm_css$Css$Structure$Compatible, textDecorationLine: $rtfeldman$elm_css$Css$Structure$Compatible, textTransform: $rtfeldman$elm_css$Css$Structure$Compatible, touchAction: $rtfeldman$elm_css$Css$Structure$Compatible, transform: $rtfeldman$elm_css$Css$Structure$Compatible, updateFrequency: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'none'};
var $rtfeldman$elm_css$Html$Styled$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var $elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 'MayStopPropagation', a: a};
};
var $rtfeldman$elm_css$Html$Styled$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			$rtfeldman$elm_css$VirtualDom$Styled$on,
			event,
			$elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $rtfeldman$elm_css$Html$Styled$Events$targetValue = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	$elm$json$Json$Decode$string);
var $rtfeldman$elm_css$Html$Styled$Events$onInput = function (tagger) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$Events$stopPropagationOn,
		'input',
		A2(
			$elm$json$Json$Decode$map,
			$rtfeldman$elm_css$Html$Styled$Events$alwaysStop,
			A2($elm$json$Json$Decode$map, tagger, $rtfeldman$elm_css$Html$Styled$Events$targetValue)));
};
var $rtfeldman$elm_css$Css$outline = $rtfeldman$elm_css$Css$prop1('outline');
var $rtfeldman$elm_css$Css$overflowY = $rtfeldman$elm_css$Css$prop1('overflow-y');
var $rtfeldman$elm_css$Css$padding = $rtfeldman$elm_css$Css$prop1('padding');
var $rtfeldman$elm_css$Html$Styled$Attributes$placeholder = $rtfeldman$elm_css$Html$Styled$Attributes$stringProperty('placeholder');
var $rtfeldman$elm_css$Css$position = $rtfeldman$elm_css$Css$prop1('position');
var $rtfeldman$elm_css$Css$relative = {position: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'relative'};
var $rtfeldman$elm_css$Css$right = $rtfeldman$elm_css$Css$prop1('right');
var $rtfeldman$elm_css$Css$solid = {borderStyle: $rtfeldman$elm_css$Css$Structure$Compatible, textDecorationStyle: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'solid'};
var $rtfeldman$elm_css$Css$spaceBetween = $rtfeldman$elm_css$Css$prop1('space-between');
var $rtfeldman$elm_css$Css$top = $rtfeldman$elm_css$Css$prop1('top');
var $rtfeldman$elm_css$Css$transparent = {color: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'transparent'};
var $rtfeldman$elm_css$Html$Styled$Attributes$value = $rtfeldman$elm_css$Html$Styled$Attributes$stringProperty('value');
var $rtfeldman$elm_css$Css$zIndex = $rtfeldman$elm_css$Css$prop1('z-index');
var $author$project$Main$customMatchupUI = function (model) {
	var searchInput = F6(
		function (searchValue, onInput, results, onSelect, placeholder, selectedPlayer) {
			return A2(
				$rtfeldman$elm_css$Html$Styled$div,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Attributes$css(
						_List_fromArray(
							[
								$rtfeldman$elm_css$Css$position($rtfeldman$elm_css$Css$relative),
								$rtfeldman$elm_css$Css$marginBottom(
								$rtfeldman$elm_css$Css$px(16))
							]))
					]),
				_List_fromArray(
					[
						A2(
						$rtfeldman$elm_css$Html$Styled$input,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$Attributes$css(
								_List_fromArray(
									[
										$rtfeldman$elm_css$Css$width(
										$rtfeldman$elm_css$Css$pct(100)),
										$rtfeldman$elm_css$Css$padding(
										$rtfeldman$elm_css$Css$px(12)),
										A3(
										$rtfeldman$elm_css$Css$border3,
										$rtfeldman$elm_css$Css$px(2),
										$rtfeldman$elm_css$Css$solid,
										function () {
											if (selectedPlayer.$ === 'Just') {
												return $rtfeldman$elm_css$Css$hex('10B981');
											} else {
												return $rtfeldman$elm_css$Css$hex('D1D5DB');
											}
										}()),
										$rtfeldman$elm_css$Css$borderRadius(
										$rtfeldman$elm_css$Css$px(8)),
										$rtfeldman$elm_css$Css$fontSize(
										$rtfeldman$elm_css$Css$px(16)),
										$author$project$Main$modernSansSerif,
										$rtfeldman$elm_css$Css$boxSizing($rtfeldman$elm_css$Css$borderBox),
										$rtfeldman$elm_css$Css$focus(
										_List_fromArray(
											[
												$rtfeldman$elm_css$Css$outline($rtfeldman$elm_css$Css$none),
												$rtfeldman$elm_css$Css$borderColor(
												$rtfeldman$elm_css$Css$hex('3B82F6'))
											]))
									])),
								$rtfeldman$elm_css$Html$Styled$Attributes$value(searchValue),
								$rtfeldman$elm_css$Html$Styled$Attributes$placeholder(placeholder),
								$rtfeldman$elm_css$Html$Styled$Events$onInput(onInput)
							]),
						_List_Nil),
						($elm$core$List$length(results) > 0) ? A2(
						$rtfeldman$elm_css$Html$Styled$div,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$Attributes$css(
								_List_fromArray(
									[
										$rtfeldman$elm_css$Css$position($rtfeldman$elm_css$Css$absolute),
										$rtfeldman$elm_css$Css$top(
										$rtfeldman$elm_css$Css$pct(100)),
										$rtfeldman$elm_css$Css$left($rtfeldman$elm_css$Css$zero),
										$rtfeldman$elm_css$Css$right($rtfeldman$elm_css$Css$zero),
										$rtfeldman$elm_css$Css$backgroundColor(
										$rtfeldman$elm_css$Css$hex('FFF')),
										A3(
										$rtfeldman$elm_css$Css$border3,
										$rtfeldman$elm_css$Css$px(1),
										$rtfeldman$elm_css$Css$solid,
										$rtfeldman$elm_css$Css$hex('E5E7EB')),
										$rtfeldman$elm_css$Css$borderRadius(
										$rtfeldman$elm_css$Css$px(8)),
										A5(
										$rtfeldman$elm_css$Css$boxShadow5,
										$rtfeldman$elm_css$Css$zero,
										$rtfeldman$elm_css$Css$px(4),
										$rtfeldman$elm_css$Css$px(6),
										$rtfeldman$elm_css$Css$px(-1),
										A4($rtfeldman$elm_css$Css$rgba, 0, 0, 0, 0.1)),
										$rtfeldman$elm_css$Css$zIndex(
										$rtfeldman$elm_css$Css$int(10)),
										$rtfeldman$elm_css$Css$maxHeight(
										$rtfeldman$elm_css$Css$px(200)),
										$rtfeldman$elm_css$Css$overflowY($rtfeldman$elm_css$Css$auto)
									]))
							]),
						A2(
							$elm$core$List$map,
							function (player) {
								return A2(
									$rtfeldman$elm_css$Html$Styled$button,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$css(
											_List_fromArray(
												[
													$rtfeldman$elm_css$Css$width(
													$rtfeldman$elm_css$Css$pct(100)),
													$rtfeldman$elm_css$Css$padding(
													$rtfeldman$elm_css$Css$px(12)),
													$rtfeldman$elm_css$Css$textAlign($rtfeldman$elm_css$Css$left),
													$rtfeldman$elm_css$Css$border($rtfeldman$elm_css$Css$zero),
													$rtfeldman$elm_css$Css$backgroundColor($rtfeldman$elm_css$Css$transparent),
													$rtfeldman$elm_css$Css$cursor($rtfeldman$elm_css$Css$pointer),
													$rtfeldman$elm_css$Css$fontSize(
													$rtfeldman$elm_css$Css$px(14)),
													$author$project$Main$modernSansSerif,
													$rtfeldman$elm_css$Css$hover(
													_List_fromArray(
														[
															$rtfeldman$elm_css$Css$backgroundColor(
															$rtfeldman$elm_css$Css$hex('F3F4F6'))
														])),
													$rtfeldman$elm_css$Css$displayFlex,
													$rtfeldman$elm_css$Css$justifyContent($rtfeldman$elm_css$Css$spaceBetween)
												])),
											$rtfeldman$elm_css$Html$Styled$Events$onClick(
											onSelect(player))
										]),
									_List_fromArray(
										[
											A2(
											$rtfeldman$elm_css$Html$Styled$span,
											_List_Nil,
											_List_fromArray(
												[
													$rtfeldman$elm_css$Html$Styled$text(
													$author$project$Player$name(player))
												])),
											A2(
											$rtfeldman$elm_css$Html$Styled$span,
											_List_fromArray(
												[
													$rtfeldman$elm_css$Html$Styled$Attributes$css(
													_List_fromArray(
														[
															$rtfeldman$elm_css$Css$color(
															$rtfeldman$elm_css$Css$hex('6B7280'))
														]))
												]),
											_List_fromArray(
												[
													$rtfeldman$elm_css$Html$Styled$text(
													$elm$core$String$fromInt(
														$author$project$Player$rating(player)))
												]))
										]));
							},
							results)) : $rtfeldman$elm_css$Html$Styled$text('')
					]));
		});
	return A2(
		$rtfeldman$elm_css$Html$Styled$div,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$Attributes$css(
				_List_fromArray(
					[
						$rtfeldman$elm_css$Css$backgroundColor(
						$rtfeldman$elm_css$Css$hex('F9FAFB')),
						A3(
						$rtfeldman$elm_css$Css$border3,
						$rtfeldman$elm_css$Css$px(1),
						$rtfeldman$elm_css$Css$solid,
						$rtfeldman$elm_css$Css$hex('E5E7EB')),
						$rtfeldman$elm_css$Css$borderRadius(
						$rtfeldman$elm_css$Css$px(12)),
						$rtfeldman$elm_css$Css$padding(
						$rtfeldman$elm_css$Css$px(24)),
						A2(
						$rtfeldman$elm_css$Css$margin2,
						$rtfeldman$elm_css$Css$px(24),
						$rtfeldman$elm_css$Css$auto),
						$rtfeldman$elm_css$Css$maxWidth(
						$rtfeldman$elm_css$Css$px(500)),
						$author$project$Main$modernSansSerif
					]))
			]),
		_List_fromArray(
			[
				A2(
				$rtfeldman$elm_css$Html$Styled$div,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Attributes$css(
						_List_fromArray(
							[
								$rtfeldman$elm_css$Css$displayFlex,
								$rtfeldman$elm_css$Css$justifyContent($rtfeldman$elm_css$Css$spaceBetween),
								$rtfeldman$elm_css$Css$alignItems($rtfeldman$elm_css$Css$center),
								$rtfeldman$elm_css$Css$marginBottom(
								$rtfeldman$elm_css$Css$px(24))
							]))
					]),
				_List_fromArray(
					[
						A2(
						$rtfeldman$elm_css$Html$Styled$h3,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$Attributes$css(
								_List_fromArray(
									[
										$rtfeldman$elm_css$Css$margin($rtfeldman$elm_css$Css$zero),
										$rtfeldman$elm_css$Css$fontSize(
										$rtfeldman$elm_css$Css$px(20)),
										$rtfeldman$elm_css$Css$fontWeight(
										$rtfeldman$elm_css$Css$int(600))
									]))
							]),
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$text('Custom Match')
							])),
						A2(
						$rtfeldman$elm_css$Html$Styled$button,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$Attributes$css(
								_List_fromArray(
									[
										$rtfeldman$elm_css$Css$backgroundColor($rtfeldman$elm_css$Css$transparent),
										$rtfeldman$elm_css$Css$border($rtfeldman$elm_css$Css$zero),
										$rtfeldman$elm_css$Css$fontSize(
										$rtfeldman$elm_css$Css$px(24)),
										$rtfeldman$elm_css$Css$cursor($rtfeldman$elm_css$Css$pointer),
										$rtfeldman$elm_css$Css$color(
										$rtfeldman$elm_css$Css$hex('6B7280')),
										$rtfeldman$elm_css$Css$hover(
										_List_fromArray(
											[
												$rtfeldman$elm_css$Css$color(
												$rtfeldman$elm_css$Css$hex('374151'))
											]))
									])),
								$rtfeldman$elm_css$Html$Styled$Events$onClick($author$project$Main$KeeperWantsToHideCustomMatchup)
							]),
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$text('×')
							]))
					])),
				A2(
				$rtfeldman$elm_css$Html$Styled$div,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Attributes$css(
						_List_fromArray(
							[
								$rtfeldman$elm_css$Css$marginBottom(
								$rtfeldman$elm_css$Css$px(20))
							]))
					]),
				_List_fromArray(
					[
						A2(
						$rtfeldman$elm_css$Html$Styled$label,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$Attributes$css(
								_List_fromArray(
									[
										$rtfeldman$elm_css$Css$display($rtfeldman$elm_css$Css$block),
										$rtfeldman$elm_css$Css$fontSize(
										$rtfeldman$elm_css$Css$px(14)),
										$rtfeldman$elm_css$Css$fontWeight(
										$rtfeldman$elm_css$Css$int(600)),
										$rtfeldman$elm_css$Css$marginBottom(
										$rtfeldman$elm_css$Css$px(8)),
										$rtfeldman$elm_css$Css$color(
										$rtfeldman$elm_css$Css$hex('374151'))
									]))
							]),
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$text('Player A')
							])),
						A6(searchInput, model.playerASearch, $author$project$Main$KeeperUpdatedPlayerASearch, model.playerASearchResults, $author$project$Main$KeeperSelectedPlayerA, 'Search for first player...', model.customMatchupPlayerA)
					])),
				A2(
				$rtfeldman$elm_css$Html$Styled$div,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Attributes$css(
						_List_fromArray(
							[
								$rtfeldman$elm_css$Css$marginBottom(
								$rtfeldman$elm_css$Css$px(24))
							]))
					]),
				_List_fromArray(
					[
						A2(
						$rtfeldman$elm_css$Html$Styled$label,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$Attributes$css(
								_List_fromArray(
									[
										$rtfeldman$elm_css$Css$display($rtfeldman$elm_css$Css$block),
										$rtfeldman$elm_css$Css$fontSize(
										$rtfeldman$elm_css$Css$px(14)),
										$rtfeldman$elm_css$Css$fontWeight(
										$rtfeldman$elm_css$Css$int(600)),
										$rtfeldman$elm_css$Css$marginBottom(
										$rtfeldman$elm_css$Css$px(8)),
										$rtfeldman$elm_css$Css$color(
										$rtfeldman$elm_css$Css$hex('374151'))
									]))
							]),
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$text('Player B')
							])),
						A6(searchInput, model.playerBSearch, $author$project$Main$KeeperUpdatedPlayerBSearch, model.playerBSearchResults, $author$project$Main$KeeperSelectedPlayerB, 'Search for second player...', model.customMatchupPlayerB)
					])),
				A2(
				$rtfeldman$elm_css$Html$Styled$div,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Attributes$css(
						_List_fromArray(
							[
								$rtfeldman$elm_css$Css$textAlign($rtfeldman$elm_css$Css$center)
							]))
					]),
				_List_fromArray(
					[
						function () {
						var _v0 = _Utils_Tuple2(model.customMatchupPlayerA, model.customMatchupPlayerB);
						if ((_v0.a.$ === 'Just') && (_v0.b.$ === 'Just')) {
							var playerA = _v0.a.a;
							var playerB = _v0.b.a;
							return _Utils_eq(
								$author$project$Player$id(playerA),
								$author$project$Player$id(playerB)) ? A2(
								$rtfeldman$elm_css$Html$Styled$div,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$Attributes$css(
										_List_fromArray(
											[
												$rtfeldman$elm_css$Css$color(
												$rtfeldman$elm_css$Css$hex('EF4444')),
												$rtfeldman$elm_css$Css$fontSize(
												$rtfeldman$elm_css$Css$px(14)),
												$rtfeldman$elm_css$Css$marginBottom(
												$rtfeldman$elm_css$Css$px(16))
											]))
									]),
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$text('Please select two different players')
									])) : A2(
								$rtfeldman$elm_css$Html$Styled$div,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$rtfeldman$elm_css$Html$Styled$div,
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$Attributes$css(
												_List_fromArray(
													[
														$rtfeldman$elm_css$Css$marginBottom(
														$rtfeldman$elm_css$Css$px(16)),
														$rtfeldman$elm_css$Css$fontSize(
														$rtfeldman$elm_css$Css$px(16)),
														$rtfeldman$elm_css$Css$color(
														$rtfeldman$elm_css$Css$hex('374151')),
														$rtfeldman$elm_css$Css$fontWeight(
														$rtfeldman$elm_css$Css$int(500))
													]))
											]),
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$text(
												$author$project$Player$name(playerA) + (' vs ' + $author$project$Player$name(playerB)))
											])),
										A2(
										$rtfeldman$elm_css$Html$Styled$button,
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$Attributes$css(
												_List_fromArray(
													[
														$rtfeldman$elm_css$Css$backgroundColor(
														$rtfeldman$elm_css$Css$hex('10B981')),
														$rtfeldman$elm_css$Css$color(
														$rtfeldman$elm_css$Css$hex('FFF')),
														$rtfeldman$elm_css$Css$border($rtfeldman$elm_css$Css$zero),
														A2(
														$rtfeldman$elm_css$Css$padding2,
														$rtfeldman$elm_css$Css$px(12),
														$rtfeldman$elm_css$Css$px(24)),
														$rtfeldman$elm_css$Css$borderRadius(
														$rtfeldman$elm_css$Css$px(8)),
														$rtfeldman$elm_css$Css$fontSize(
														$rtfeldman$elm_css$Css$px(16)),
														$rtfeldman$elm_css$Css$fontWeight(
														$rtfeldman$elm_css$Css$int(600)),
														$rtfeldman$elm_css$Css$cursor($rtfeldman$elm_css$Css$pointer),
														$author$project$Main$modernSansSerif,
														$rtfeldman$elm_css$Css$hover(
														_List_fromArray(
															[
																$rtfeldman$elm_css$Css$backgroundColor(
																$rtfeldman$elm_css$Css$hex('059669'))
															]))
													])),
												$rtfeldman$elm_css$Html$Styled$Events$onClick($author$project$Main$KeeperWantsToStartCustomMatch)
											]),
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$text('Start Match')
											]))
									]));
						} else {
							return A2(
								$rtfeldman$elm_css$Html$Styled$div,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$Attributes$css(
										_List_fromArray(
											[
												$rtfeldman$elm_css$Css$color(
												$rtfeldman$elm_css$Css$hex('6B7280')),
												$rtfeldman$elm_css$Css$fontSize(
												$rtfeldman$elm_css$Css$px(14))
											]))
									]),
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$text('Search and select both players to start the match')
									]));
						}
					}()
					]))
			]));
};
var $rtfeldman$elm_css$Css$flexGrow = $rtfeldman$elm_css$Css$prop1('flex-grow');
var $author$project$Main$greenButton = $author$project$Main$button(
	$rtfeldman$elm_css$Css$hex('6DD400'));
var $rtfeldman$elm_css$Html$Styled$h1 = $rtfeldman$elm_css$Html$Styled$node('h1');
var $rtfeldman$elm_css$Css$height = $rtfeldman$elm_css$Css$prop1('height');
var $rtfeldman$elm_css$Css$hidden = {borderStyle: $rtfeldman$elm_css$Css$Structure$Compatible, overflow: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'hidden', visibility: $rtfeldman$elm_css$Css$Structure$Compatible};
var $rtfeldman$elm_css$Css$lineHeight = $rtfeldman$elm_css$Css$prop1('line-height');
var $rtfeldman$elm_css$Css$marginLeft = $rtfeldman$elm_css$Css$prop1('margin-left');
var $rtfeldman$elm_css$Css$marginTop = $rtfeldman$elm_css$Css$prop1('margin-top');
var $rtfeldman$elm_css$Css$Media$feature = F2(
	function (key, _v0) {
		var value = _v0.value;
		return {
			feature: key,
			value: $elm$core$Maybe$Just(value)
		};
	});
var $rtfeldman$elm_css$Css$Media$maxWidth = function (value) {
	return A2($rtfeldman$elm_css$Css$Media$feature, 'max-width', value);
};
var $rtfeldman$elm_css$Css$UnitlessFloat = {$: 'UnitlessFloat'};
var $rtfeldman$elm_css$Css$num = function (val) {
	return {
		lengthOrNumber: $rtfeldman$elm_css$Css$Structure$Compatible,
		lengthOrNumberOrAutoOrNoneOrContent: $rtfeldman$elm_css$Css$Structure$Compatible,
		number: $rtfeldman$elm_css$Css$Structure$Compatible,
		numberOrInfinite: $rtfeldman$elm_css$Css$Structure$Compatible,
		numericValue: val,
		unitLabel: '',
		units: $rtfeldman$elm_css$Css$UnitlessFloat,
		value: $elm$core$String$fromFloat(val)
	};
};
var $rtfeldman$elm_css$Css$Structure$OnlyQuery = F2(
	function (a, b) {
		return {$: 'OnlyQuery', a: a, b: b};
	});
var $rtfeldman$elm_css$Css$Media$only = $rtfeldman$elm_css$Css$Structure$OnlyQuery;
var $rtfeldman$elm_css$Css$overflow = $rtfeldman$elm_css$Css$prop1('overflow');
var $rtfeldman$elm_css$Html$Styled$p = $rtfeldman$elm_css$Html$Styled$node('p');
var $rtfeldman$elm_css$Css$padding4 = $rtfeldman$elm_css$Css$prop4('padding');
var $author$project$History$peekBack = function (_v0) {
	var guts = _v0.a;
	return $elm$core$List$head(guts.past);
};
var $author$project$History$peekForward = function (_v0) {
	var guts = _v0.a;
	return $elm$core$List$head(guts.future);
};
var $author$project$Main$redButton = $author$project$Main$button(
	$rtfeldman$elm_css$Css$hex('EF4444'));
var $author$project$Main$redButtonLarge = $author$project$Main$buttonLarge(
	$rtfeldman$elm_css$Css$hex('EF4444'));
var $rtfeldman$elm_css$Css$Structure$Screen = {$: 'Screen'};
var $rtfeldman$elm_css$Css$Media$screen = $rtfeldman$elm_css$Css$Structure$Screen;
var $rtfeldman$elm_css$Html$Styled$section = $rtfeldman$elm_css$Html$Styled$node('section');
var $rtfeldman$elm_css$Css$textShadow4 = $rtfeldman$elm_css$Css$prop4('text-shadow');
var $rtfeldman$elm_css$Css$Preprocess$WithMedia = F2(
	function (a, b) {
		return {$: 'WithMedia', a: a, b: b};
	});
var $rtfeldman$elm_css$Css$Media$withMedia = $rtfeldman$elm_css$Css$Preprocess$WithMedia;
var $rtfeldman$elm_css$Css$active = $rtfeldman$elm_css$Css$pseudoClass('active');
var $rtfeldman$elm_css$Css$outline3 = $rtfeldman$elm_css$Css$prop3('outline');
var $rtfeldman$elm_css$Css$outlineOffset = $rtfeldman$elm_css$Css$prop1('outline-offset');
var $author$project$Main$zzzIgnoreButton = function (maybeMsg) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$button,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$Attributes$css(
				_List_fromArray(
					[
						$rtfeldman$elm_css$Css$paddingTop(
						$rtfeldman$elm_css$Css$px(4)),
						$rtfeldman$elm_css$Css$paddingBottom(
						$rtfeldman$elm_css$Css$px(6)),
						$rtfeldman$elm_css$Css$paddingLeft(
						$rtfeldman$elm_css$Css$px(12)),
						$rtfeldman$elm_css$Css$paddingRight(
						$rtfeldman$elm_css$Css$px(12)),
						A2(
						$rtfeldman$elm_css$Css$margin2,
						$rtfeldman$elm_css$Css$zero,
						$rtfeldman$elm_css$Css$px(6)),
						$rtfeldman$elm_css$Css$minWidth(
						$rtfeldman$elm_css$Css$px(44)),
						$rtfeldman$elm_css$Css$backgroundColor(
						$rtfeldman$elm_css$Css$hex('6B7280')),
						$rtfeldman$elm_css$Css$border($rtfeldman$elm_css$Css$zero),
						$rtfeldman$elm_css$Css$borderRadius(
						$rtfeldman$elm_css$Css$px(9999)),
						$rtfeldman$elm_css$Css$cursor($rtfeldman$elm_css$Css$pointer),
						$rtfeldman$elm_css$Css$fontSize(
						$rtfeldman$elm_css$Css$px(13)),
						$rtfeldman$elm_css$Css$fontWeight(
						$rtfeldman$elm_css$Css$int(600)),
						$rtfeldman$elm_css$Css$color(
						$rtfeldman$elm_css$Css$hex('FFF')),
						$author$project$Main$modernSansSerif,
						$rtfeldman$elm_css$Css$hover(
						_List_fromArray(
							[
								$rtfeldman$elm_css$Css$backgroundColor(
								$rtfeldman$elm_css$Css$hex('4B5563'))
							])),
						$rtfeldman$elm_css$Css$active(
						_List_fromArray(
							[
								$rtfeldman$elm_css$Css$backgroundColor(
								$rtfeldman$elm_css$Css$hex('374151'))
							])),
						$rtfeldman$elm_css$Css$focus(
						_List_fromArray(
							[
								A3(
								$rtfeldman$elm_css$Css$outline3,
								$rtfeldman$elm_css$Css$px(2),
								$rtfeldman$elm_css$Css$solid,
								$rtfeldman$elm_css$Css$hex('93C5FD')),
								$rtfeldman$elm_css$Css$outlineOffset(
								$rtfeldman$elm_css$Css$px(2))
							]))
					])),
				function () {
				if (maybeMsg.$ === 'Just') {
					var m = maybeMsg.a;
					return $rtfeldman$elm_css$Html$Styled$Events$onClick(m);
				} else {
					return $rtfeldman$elm_css$Html$Styled$Attributes$disabled(true);
				}
			}()
			]),
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$text('Zzz')
			]));
};
var $author$project$Main$zzzUnignoreButton = function (maybeMsg) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$button,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$Attributes$css(
				_List_fromArray(
					[
						$rtfeldman$elm_css$Css$paddingTop(
						$rtfeldman$elm_css$Css$px(4)),
						$rtfeldman$elm_css$Css$paddingBottom(
						$rtfeldman$elm_css$Css$px(6)),
						$rtfeldman$elm_css$Css$paddingLeft(
						$rtfeldman$elm_css$Css$px(12)),
						$rtfeldman$elm_css$Css$paddingRight(
						$rtfeldman$elm_css$Css$px(12)),
						A2(
						$rtfeldman$elm_css$Css$margin2,
						$rtfeldman$elm_css$Css$zero,
						$rtfeldman$elm_css$Css$px(6)),
						$rtfeldman$elm_css$Css$minWidth(
						$rtfeldman$elm_css$Css$px(44)),
						$rtfeldman$elm_css$Css$backgroundColor(
						$rtfeldman$elm_css$Css$hex('374151')),
						$rtfeldman$elm_css$Css$border($rtfeldman$elm_css$Css$zero),
						$rtfeldman$elm_css$Css$borderRadius(
						$rtfeldman$elm_css$Css$px(9999)),
						$rtfeldman$elm_css$Css$cursor($rtfeldman$elm_css$Css$pointer),
						$rtfeldman$elm_css$Css$fontSize(
						$rtfeldman$elm_css$Css$px(13)),
						$rtfeldman$elm_css$Css$fontWeight(
						$rtfeldman$elm_css$Css$int(600)),
						$rtfeldman$elm_css$Css$color(
						$rtfeldman$elm_css$Css$hex('FFF')),
						$rtfeldman$elm_css$Css$textDecoration($rtfeldman$elm_css$Css$lineThrough),
						$author$project$Main$modernSansSerif,
						$rtfeldman$elm_css$Css$hover(
						_List_fromArray(
							[
								$rtfeldman$elm_css$Css$backgroundColor(
								$rtfeldman$elm_css$Css$hex('1F2937'))
							])),
						$rtfeldman$elm_css$Css$active(
						_List_fromArray(
							[
								$rtfeldman$elm_css$Css$backgroundColor(
								$rtfeldman$elm_css$Css$hex('111827'))
							])),
						$rtfeldman$elm_css$Css$focus(
						_List_fromArray(
							[
								A3(
								$rtfeldman$elm_css$Css$outline3,
								$rtfeldman$elm_css$Css$px(2),
								$rtfeldman$elm_css$Css$solid,
								$rtfeldman$elm_css$Css$hex('93C5FD')),
								$rtfeldman$elm_css$Css$outlineOffset(
								$rtfeldman$elm_css$Css$px(2))
							]))
					])),
				function () {
				if (maybeMsg.$ === 'Just') {
					var m = maybeMsg.a;
					return $rtfeldman$elm_css$Html$Styled$Events$onClick(m);
				} else {
					return $rtfeldman$elm_css$Html$Styled$Attributes$disabled(true);
				}
			}()
			]),
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$text('Zzz')
			]));
};
var $author$project$Main$currentMatch = function (model) {
	var _v0 = $author$project$League$currentMatch(
		$author$project$History$current(model.history));
	if (_v0.$ === 'Nothing') {
		return A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$css(
					_List_fromArray(
						[
							$author$project$Main$modernSansSerif,
							$rtfeldman$elm_css$Css$textAlign($rtfeldman$elm_css$Css$center),
							$rtfeldman$elm_css$Css$width(
							$rtfeldman$elm_css$Css$pct(50)),
							A2(
							$rtfeldman$elm_css$Css$margin2,
							$rtfeldman$elm_css$Css$px(32),
							$rtfeldman$elm_css$Css$auto)
						]))
				]),
			_List_fromArray(
				[
					A2(
					$rtfeldman$elm_css$Html$Styled$h1,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$css(
							_List_fromArray(
								[
									$rtfeldman$elm_css$Css$fontSize(
									$rtfeldman$elm_css$Css$px(56)),
									$rtfeldman$elm_css$Css$marginBottom(
									$rtfeldman$elm_css$Css$px(18)),
									$rtfeldman$elm_css$Css$fontWeight(
									$rtfeldman$elm_css$Css$int(700)),
									$rtfeldman$elm_css$Css$fontStyle($rtfeldman$elm_css$Css$italic),
									$rtfeldman$elm_css$Css$textTransform($rtfeldman$elm_css$Css$uppercase)
								]))
						]),
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text('Hockey Rater 🏒')
						])),
					A2(
					$rtfeldman$elm_css$Html$Styled$p,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$css(
							_List_fromArray(
								[
									$rtfeldman$elm_css$Css$fontSize(
									$rtfeldman$elm_css$Css$px(24)),
									$rtfeldman$elm_css$Css$lineHeight(
									$rtfeldman$elm_css$Css$px(32)),
									$rtfeldman$elm_css$Css$marginBottom(
									$rtfeldman$elm_css$Css$px(18))
								]))
						]),
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text('No current match. To get started, add at least two players!')
						]))
				]));
	} else {
		var _v1 = _v0.a;
		var playerA = _v1.a;
		var playerB = _v1.b;
		var chanceAWins = A2(
			$author$project$Elo$odds,
			$author$project$Player$rating(playerA),
			$author$project$Player$rating(playerB));
		return A2(
			$rtfeldman$elm_css$Html$Styled$section,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$css(
					_List_fromArray(
						[
							$rtfeldman$elm_css$Css$width(
							$rtfeldman$elm_css$Css$pct(80)),
							A2(
							$rtfeldman$elm_css$Css$margin2,
							$rtfeldman$elm_css$Css$px(32),
							$rtfeldman$elm_css$Css$auto)
						]))
				]),
			_List_fromArray(
				[
					A2(
					$rtfeldman$elm_css$Html$Styled$div,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$css(
							_List_fromArray(
								[
									$rtfeldman$elm_css$Css$position($rtfeldman$elm_css$Css$relative),
									$rtfeldman$elm_css$Css$marginBottom(
									$rtfeldman$elm_css$Css$px(20))
								]))
						]),
					_List_fromArray(
						[
							A2(
							$rtfeldman$elm_css$Html$Styled$div,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$css(
									_List_fromArray(
										[
											$rtfeldman$elm_css$Css$fontSize(
											$rtfeldman$elm_css$Css$px(14)),
											$rtfeldman$elm_css$Css$fontWeight(
											$rtfeldman$elm_css$Css$int(700)),
											$rtfeldman$elm_css$Css$textAlign($rtfeldman$elm_css$Css$center),
											$rtfeldman$elm_css$Css$marginBottom(
											$rtfeldman$elm_css$Css$px(12)),
											$rtfeldman$elm_css$Css$color(
											$rtfeldman$elm_css$Css$hex('555')),
											$rtfeldman$elm_css$Css$letterSpacing(
											$rtfeldman$elm_css$Css$px(1)),
											$rtfeldman$elm_css$Css$fontStyle($rtfeldman$elm_css$Css$italic),
											$rtfeldman$elm_css$Css$textTransform($rtfeldman$elm_css$Css$uppercase),
											$author$project$Main$modernSansSerif
										]))
								]),
							_List_fromArray(
								[
									A2(
									$rtfeldman$elm_css$Html$Styled$div,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$css(
											_List_fromArray(
												[
													$rtfeldman$elm_css$Css$displayFlex,
													$rtfeldman$elm_css$Css$alignItems($rtfeldman$elm_css$Css$center),
													$rtfeldman$elm_css$Css$justifyContent($rtfeldman$elm_css$Css$center)
												]))
										]),
									_List_fromArray(
										[
											A2(
											$rtfeldman$elm_css$Html$Styled$span,
											_List_fromArray(
												[
													$rtfeldman$elm_css$Html$Styled$Attributes$css(
													_List_fromArray(
														[
															$rtfeldman$elm_css$Css$marginRight(
															$rtfeldman$elm_css$Css$px(8))
														]))
												]),
											_List_fromArray(
												[
													$rtfeldman$elm_css$Html$Styled$text('🏒 HOCKEY RATER 🏒')
												]))
										]))
								])),
							A2(
							$rtfeldman$elm_css$Html$Styled$div,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$css(
									_List_fromArray(
										[
											$rtfeldman$elm_css$Css$position($rtfeldman$elm_css$Css$relative),
											$rtfeldman$elm_css$Css$borderRadius(
											$rtfeldman$elm_css$Css$px(20)),
											$rtfeldman$elm_css$Css$overflow($rtfeldman$elm_css$Css$hidden),
											$rtfeldman$elm_css$Css$height(
											$rtfeldman$elm_css$Css$px(32)),
											$rtfeldman$elm_css$Css$width(
											$rtfeldman$elm_css$Css$pct(100)),
											$rtfeldman$elm_css$Css$backgroundColor(
											$rtfeldman$elm_css$Css$hex('E8E8E8')),
											A6(
											$rtfeldman$elm_css$Css$boxShadow6,
											$rtfeldman$elm_css$Css$inset,
											$rtfeldman$elm_css$Css$px(0),
											$rtfeldman$elm_css$Css$px(3),
											$rtfeldman$elm_css$Css$px(6),
											$rtfeldman$elm_css$Css$px(0),
											A4($rtfeldman$elm_css$Css$rgba, 0, 0, 0, 0.15)),
											A3(
											$rtfeldman$elm_css$Css$border3,
											$rtfeldman$elm_css$Css$px(2),
											$rtfeldman$elm_css$Css$solid,
											$rtfeldman$elm_css$Css$hex('DDD'))
										]))
								]),
							_List_fromArray(
								[
									A2(
									$rtfeldman$elm_css$Html$Styled$div,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$css(
											_List_fromArray(
												[
													$rtfeldman$elm_css$Css$width(
													$rtfeldman$elm_css$Css$pct(
														A2(
															$elm$core$Basics$max,
															($elm$core$Basics$round(chanceAWins * 100) >= 100) ? 20 : 16,
															100 * chanceAWins))),
													$rtfeldman$elm_css$Css$height(
													$rtfeldman$elm_css$Css$pct(100)),
													$rtfeldman$elm_css$Css$backgroundColor(
													$rtfeldman$elm_css$Css$hex('EF4444')),
													$rtfeldman$elm_css$Css$position($rtfeldman$elm_css$Css$absolute),
													$rtfeldman$elm_css$Css$left(
													$rtfeldman$elm_css$Css$px(0)),
													$rtfeldman$elm_css$Css$top(
													$rtfeldman$elm_css$Css$px(0)),
													A4(
													$rtfeldman$elm_css$Css$boxShadow4,
													$rtfeldman$elm_css$Css$px(2),
													$rtfeldman$elm_css$Css$px(0),
													$rtfeldman$elm_css$Css$px(4),
													A4($rtfeldman$elm_css$Css$rgba, 0, 0, 0, 0.2))
												]))
										]),
									_List_Nil),
									A2(
									$rtfeldman$elm_css$Html$Styled$div,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$css(
											_List_fromArray(
												[
													$rtfeldman$elm_css$Css$width(
													$rtfeldman$elm_css$Css$pct(
														A2(
															$elm$core$Basics$max,
															($elm$core$Basics$round((1 - chanceAWins) * 100) >= 100) ? 20 : 16,
															100 * (1 - chanceAWins)))),
													$rtfeldman$elm_css$Css$height(
													$rtfeldman$elm_css$Css$pct(100)),
													$rtfeldman$elm_css$Css$backgroundColor(
													$rtfeldman$elm_css$Css$hex('3B82F6')),
													$rtfeldman$elm_css$Css$position($rtfeldman$elm_css$Css$absolute),
													$rtfeldman$elm_css$Css$right(
													$rtfeldman$elm_css$Css$px(0)),
													$rtfeldman$elm_css$Css$top(
													$rtfeldman$elm_css$Css$px(0)),
													A4(
													$rtfeldman$elm_css$Css$boxShadow4,
													$rtfeldman$elm_css$Css$px(-2),
													$rtfeldman$elm_css$Css$px(0),
													$rtfeldman$elm_css$Css$px(4),
													A4($rtfeldman$elm_css$Css$rgba, 0, 0, 0, 0.2))
												]))
										]),
									_List_Nil),
									A2(
									$rtfeldman$elm_css$Html$Styled$div,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$css(
											_List_fromArray(
												[
													$rtfeldman$elm_css$Css$position($rtfeldman$elm_css$Css$absolute),
													$rtfeldman$elm_css$Css$top(
													$rtfeldman$elm_css$Css$px(4)),
													$rtfeldman$elm_css$Css$left(
													$rtfeldman$elm_css$Css$px(8)),
													$rtfeldman$elm_css$Css$fontSize(
													$rtfeldman$elm_css$Css$px(16)),
													$rtfeldman$elm_css$Css$fontWeight(
													$rtfeldman$elm_css$Css$int(700)),
													$rtfeldman$elm_css$Css$fontStyle($rtfeldman$elm_css$Css$italic),
													$rtfeldman$elm_css$Css$color(
													$rtfeldman$elm_css$Css$hex('FFF')),
													A4(
													$rtfeldman$elm_css$Css$textShadow4,
													$rtfeldman$elm_css$Css$px(1),
													$rtfeldman$elm_css$Css$px(1),
													$rtfeldman$elm_css$Css$px(2),
													A4($rtfeldman$elm_css$Css$rgba, 0, 0, 0, 0.7)),
													$author$project$Main$modernSansSerif
												]))
										]),
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$text(
											$elm$core$String$fromInt(
												$elm$core$Basics$round(chanceAWins * 100)) + '%')
										])),
									A2(
									$rtfeldman$elm_css$Html$Styled$div,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$css(
											_List_fromArray(
												[
													$rtfeldman$elm_css$Css$position($rtfeldman$elm_css$Css$absolute),
													$rtfeldman$elm_css$Css$top(
													$rtfeldman$elm_css$Css$px(4)),
													$rtfeldman$elm_css$Css$right(
													$rtfeldman$elm_css$Css$px(8)),
													$rtfeldman$elm_css$Css$fontSize(
													$rtfeldman$elm_css$Css$px(16)),
													$rtfeldman$elm_css$Css$fontWeight(
													$rtfeldman$elm_css$Css$int(700)),
													$rtfeldman$elm_css$Css$fontStyle($rtfeldman$elm_css$Css$italic),
													$rtfeldman$elm_css$Css$color(
													$rtfeldman$elm_css$Css$hex('FFF')),
													A4(
													$rtfeldman$elm_css$Css$textShadow4,
													$rtfeldman$elm_css$Css$px(1),
													$rtfeldman$elm_css$Css$px(1),
													$rtfeldman$elm_css$Css$px(2),
													A4($rtfeldman$elm_css$Css$rgba, 0, 0, 0, 0.7)),
													$author$project$Main$modernSansSerif
												]))
										]),
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$text(
											$elm$core$String$fromInt(
												$elm$core$Basics$round((1 - chanceAWins) * 100)) + '%')
										]))
								]))
						])),
					A2(
					$rtfeldman$elm_css$Html$Styled$div,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$css(
							_List_fromArray(
								[
									$rtfeldman$elm_css$Css$displayFlex,
									$rtfeldman$elm_css$Css$alignItems($rtfeldman$elm_css$Css$center),
									$rtfeldman$elm_css$Css$paddingTop(
									$rtfeldman$elm_css$Css$px(32)),
									A2(
									$rtfeldman$elm_css$Css$Media$withMedia,
									_List_fromArray(
										[
											A2(
											$rtfeldman$elm_css$Css$Media$only,
											$rtfeldman$elm_css$Css$Media$screen,
											_List_fromArray(
												[
													$rtfeldman$elm_css$Css$Media$maxWidth(
													$rtfeldman$elm_css$Css$px(640))
												]))
										]),
									_List_fromArray(
										[
											$rtfeldman$elm_css$Css$display($rtfeldman$elm_css$Css$none)
										]))
								]))
						]),
					_List_fromArray(
						[
							$author$project$Main$activePlayer(playerA),
							A2(
							$rtfeldman$elm_css$Html$Styled$p,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$css(
									_List_fromArray(
										[
											$author$project$Main$modernSansSerif,
											$rtfeldman$elm_css$Css$width(
											$rtfeldman$elm_css$Css$pct(20)),
											$rtfeldman$elm_css$Css$textAlign($rtfeldman$elm_css$Css$center)
										]))
								]),
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$text('vs.')
								])),
							$author$project$Main$activePlayer(playerB)
						])),
					A2(
					$rtfeldman$elm_css$Html$Styled$div,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$css(
							_List_fromArray(
								[
									$rtfeldman$elm_css$Css$display($rtfeldman$elm_css$Css$none),
									$rtfeldman$elm_css$Css$marginTop(
									$rtfeldman$elm_css$Css$px(24)),
									A2(
									$rtfeldman$elm_css$Css$Media$withMedia,
									_List_fromArray(
										[
											A2(
											$rtfeldman$elm_css$Css$Media$only,
											$rtfeldman$elm_css$Css$Media$screen,
											_List_fromArray(
												[
													$rtfeldman$elm_css$Css$Media$maxWidth(
													$rtfeldman$elm_css$Css$px(640))
												]))
										]),
									_List_fromArray(
										[
											$rtfeldman$elm_css$Css$display($rtfeldman$elm_css$Css$block)
										]))
								]))
						]),
					_List_fromArray(
						[
							A2(
							$rtfeldman$elm_css$Html$Styled$div,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$css(
									_List_fromArray(
										[
											$rtfeldman$elm_css$Css$displayFlex,
											$rtfeldman$elm_css$Css$alignItems($rtfeldman$elm_css$Css$center),
											$rtfeldman$elm_css$Css$justifyContent($rtfeldman$elm_css$Css$spaceBetween),
											$rtfeldman$elm_css$Css$marginBottom(
											$rtfeldman$elm_css$Css$px(10))
										]))
								]),
							_List_fromArray(
								[
									A2(
									$rtfeldman$elm_css$Html$Styled$div,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$css(
											_List_fromArray(
												[
													$rtfeldman$elm_css$Css$flexGrow(
													$rtfeldman$elm_css$Css$num(1))
												]))
										]),
									_List_fromArray(
										[
											A2($author$project$Main$activePlayerCompactWithIgnore, playerA, model)
										])),
									A2(
									$rtfeldman$elm_css$Html$Styled$div,
									_List_Nil,
									_List_fromArray(
										[
											A2(
											$author$project$Main$redButtonLarge,
											'WINNER',
											$author$project$Main$isVotingDisabled(model) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
												$author$project$Main$MatchFinished(
													$author$project$League$Win(
														{lost: playerB, won: playerA}))))
										]))
								])),
							A2(
							$rtfeldman$elm_css$Html$Styled$div,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$css(
									_List_fromArray(
										[
											$rtfeldman$elm_css$Css$displayFlex,
											$rtfeldman$elm_css$Css$alignItems($rtfeldman$elm_css$Css$center),
											$rtfeldman$elm_css$Css$justifyContent($rtfeldman$elm_css$Css$spaceBetween)
										]))
								]),
							_List_fromArray(
								[
									A2(
									$rtfeldman$elm_css$Html$Styled$div,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$css(
											_List_fromArray(
												[
													$rtfeldman$elm_css$Css$flexGrow(
													$rtfeldman$elm_css$Css$num(1))
												]))
										]),
									_List_fromArray(
										[
											A2($author$project$Main$activePlayerCompactWithIgnore, playerB, model)
										])),
									A2(
									$rtfeldman$elm_css$Html$Styled$div,
									_List_Nil,
									_List_fromArray(
										[
											A2(
											$author$project$Main$blueButtonLarge,
											'WINNER',
											$author$project$Main$isVotingDisabled(model) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
												$author$project$Main$MatchFinished(
													$author$project$League$Win(
														{lost: playerA, won: playerB}))))
										]))
								])),
							A2(
							$rtfeldman$elm_css$Html$Styled$div,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$css(
									_List_fromArray(
										[
											$rtfeldman$elm_css$Css$height(
											$rtfeldman$elm_css$Css$px(4)),
											$rtfeldman$elm_css$Css$backgroundColor(
											$rtfeldman$elm_css$Css$hex('D1D5DB')),
											$rtfeldman$elm_css$Css$borderRadius(
											$rtfeldman$elm_css$Css$px(2)),
											A2(
											$rtfeldman$elm_css$Css$margin2,
											$rtfeldman$elm_css$Css$px(14),
											$rtfeldman$elm_css$Css$zero)
										]))
								]),
							_List_Nil),
							A2(
							$rtfeldman$elm_css$Html$Styled$div,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$css(
									_List_fromArray(
										[
											$rtfeldman$elm_css$Css$displayFlex,
											$rtfeldman$elm_css$Css$alignItems($rtfeldman$elm_css$Css$center),
											$rtfeldman$elm_css$Css$justifyContent($rtfeldman$elm_css$Css$center)
										]))
								]),
							_List_fromArray(
								[
									A2(
									$rtfeldman$elm_css$Html$Styled$div,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$css(
											_List_fromArray(
												[
													$rtfeldman$elm_css$Css$marginRight(
													$rtfeldman$elm_css$Css$px(4))
												]))
										]),
									_List_fromArray(
										[
											A3(
											$author$project$Main$buttonCompact,
											$rtfeldman$elm_css$Css$hex('1F2937'),
											'TIE',
											$author$project$Main$isVotingDisabled(model) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
												$author$project$Main$MatchFinished(
													$author$project$League$Draw(
														{playerA: playerA, playerB: playerB}))))
										])),
									A2(
									$rtfeldman$elm_css$Html$Styled$div,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$css(
											_List_fromArray(
												[
													$rtfeldman$elm_css$Css$marginLeft(
													$rtfeldman$elm_css$Css$px(4))
												]))
										]),
									_List_fromArray(
										[
											A3(
											$author$project$Main$buttonCompact,
											$rtfeldman$elm_css$Css$hex('999'),
											'SKIP',
											$elm$core$Maybe$Just($author$project$Main$KeeperWantsToSkipMatch))
										]))
								])),
							A2(
							$rtfeldman$elm_css$Html$Styled$div,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$css(
									_List_fromArray(
										[
											$rtfeldman$elm_css$Css$displayFlex,
											$rtfeldman$elm_css$Css$alignItems($rtfeldman$elm_css$Css$center),
											$rtfeldman$elm_css$Css$justifyContent($rtfeldman$elm_css$Css$center),
											$rtfeldman$elm_css$Css$marginTop(
											$rtfeldman$elm_css$Css$px(8))
										]))
								]),
							_List_fromArray(
								[
									A2(
									$rtfeldman$elm_css$Html$Styled$div,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$css(
											_List_fromArray(
												[
													$rtfeldman$elm_css$Css$marginRight(
													$rtfeldman$elm_css$Css$px(4))
												]))
										]),
									_List_fromArray(
										[
											A3(
											$author$project$Main$buttonCompact,
											$rtfeldman$elm_css$Css$hex('6DD400'),
											'CUSTOM',
											$elm$core$Maybe$Just($author$project$Main$KeeperWantsToShowCustomMatchup))
										])),
									A2(
									$rtfeldman$elm_css$Html$Styled$div,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$css(
											_List_fromArray(
												[
													$rtfeldman$elm_css$Css$marginLeft(
													$rtfeldman$elm_css$Css$px(4))
												]))
										]),
									_List_fromArray(
										[
											A3(
											$author$project$Main$buttonCompact,
											$rtfeldman$elm_css$Css$hex('6DD400'),
											'ADD PLAYER',
											$elm$core$Maybe$Just($author$project$Main$KeeperWantsToAddNewPlayer))
										]))
								]))
						])),
					A2(
					$rtfeldman$elm_css$Html$Styled$div,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$css(
							_List_fromArray(
								[
									$rtfeldman$elm_css$Css$displayFlex,
									$rtfeldman$elm_css$Css$paddingTop(
									$rtfeldman$elm_css$Css$px(32)),
									$rtfeldman$elm_css$Css$textAlign($rtfeldman$elm_css$Css$center),
									A2(
									$rtfeldman$elm_css$Css$Media$withMedia,
									_List_fromArray(
										[
											A2(
											$rtfeldman$elm_css$Css$Media$only,
											$rtfeldman$elm_css$Css$Media$screen,
											_List_fromArray(
												[
													$rtfeldman$elm_css$Css$Media$maxWidth(
													$rtfeldman$elm_css$Css$px(640))
												]))
										]),
									_List_fromArray(
										[
											$rtfeldman$elm_css$Css$display($rtfeldman$elm_css$Css$none)
										]))
								]))
						]),
					_List_fromArray(
						[
							A2(
							$rtfeldman$elm_css$Html$Styled$div,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$css(
									_List_fromArray(
										[
											$rtfeldman$elm_css$Css$width(
											$rtfeldman$elm_css$Css$pct(40))
										]))
								]),
							_List_fromArray(
								[
									A2(
									$author$project$Main$redButton,
									'WINNER',
									$author$project$Main$isVotingDisabled(model) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
										$author$project$Main$MatchFinished(
											$author$project$League$Win(
												{lost: playerB, won: playerA}))))
								])),
							A2(
							$rtfeldman$elm_css$Html$Styled$div,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$css(
									_List_fromArray(
										[
											$rtfeldman$elm_css$Css$width(
											$rtfeldman$elm_css$Css$pct(20))
										]))
								]),
							_List_fromArray(
								[
									A2(
									$author$project$Main$blackButton,
									'TIE',
									$author$project$Main$isVotingDisabled(model) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
										$author$project$Main$MatchFinished(
											$author$project$League$Draw(
												{playerA: playerA, playerB: playerB}))))
								])),
							A2(
							$rtfeldman$elm_css$Html$Styled$div,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$css(
									_List_fromArray(
										[
											$rtfeldman$elm_css$Css$width(
											$rtfeldman$elm_css$Css$pct(40))
										]))
								]),
							_List_fromArray(
								[
									A2(
									$author$project$Main$blueButton,
									'WINNER',
									$author$project$Main$isVotingDisabled(model) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
										$author$project$Main$MatchFinished(
											$author$project$League$Win(
												{lost: playerA, won: playerB}))))
								]))
						])),
					A2(
					$rtfeldman$elm_css$Html$Styled$div,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$css(
							_List_fromArray(
								[
									$rtfeldman$elm_css$Css$textAlign($rtfeldman$elm_css$Css$center),
									$rtfeldman$elm_css$Css$marginTop(
									$rtfeldman$elm_css$Css$px(8)),
									$author$project$Main$modernSansSerif,
									$rtfeldman$elm_css$Css$color(
									$rtfeldman$elm_css$Css$hex('6B7280')),
									$rtfeldman$elm_css$Css$fontSize(
									$rtfeldman$elm_css$Css$px(12)),
									$rtfeldman$elm_css$Css$letterSpacing(
									$rtfeldman$elm_css$Css$px(0.5)),
									A2(
									$rtfeldman$elm_css$Css$Media$withMedia,
									_List_fromArray(
										[
											A2(
											$rtfeldman$elm_css$Css$Media$only,
											$rtfeldman$elm_css$Css$Media$screen,
											_List_fromArray(
												[
													$rtfeldman$elm_css$Css$Media$maxWidth(
													$rtfeldman$elm_css$Css$px(640))
												]))
										]),
									_List_fromArray(
										[
											$rtfeldman$elm_css$Css$display($rtfeldman$elm_css$Css$none)
										]))
								]))
						]),
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text('Shortcuts: Left (1) • Right (2) • Tie (0) • Skip (Esc) • Undo (Backspace)')
						])),
					A2(
					$rtfeldman$elm_css$Html$Styled$div,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$css(
							_List_fromArray(
								[
									$rtfeldman$elm_css$Css$displayFlex,
									$rtfeldman$elm_css$Css$justifyContent($rtfeldman$elm_css$Css$spaceBetween),
									$rtfeldman$elm_css$Css$paddingTop(
									$rtfeldman$elm_css$Css$px(12))
								]))
						]),
					_List_fromArray(
						[
							A2(
							$rtfeldman$elm_css$Html$Styled$div,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$css(
									_List_fromArray(
										[
											$rtfeldman$elm_css$Css$width(
											$rtfeldman$elm_css$Css$pct(40)),
											$rtfeldman$elm_css$Css$textAlign($rtfeldman$elm_css$Css$center),
											A2(
											$rtfeldman$elm_css$Css$Media$withMedia,
											_List_fromArray(
												[
													A2(
													$rtfeldman$elm_css$Css$Media$only,
													$rtfeldman$elm_css$Css$Media$screen,
													_List_fromArray(
														[
															$rtfeldman$elm_css$Css$Media$maxWidth(
															$rtfeldman$elm_css$Css$px(640))
														]))
												]),
											_List_fromArray(
												[
													$rtfeldman$elm_css$Css$display($rtfeldman$elm_css$Css$none)
												]))
										]))
								]),
							A2($author$project$Main$isPlayerLocallyIgnored, playerA, model) ? _List_fromArray(
								[
									$author$project$Main$zzzUnignoreButton(
									$elm$core$Maybe$Just(
										$author$project$Main$KeeperWantsToUnignorePlayer(playerA)))
								]) : _List_fromArray(
								[
									$author$project$Main$zzzIgnoreButton(
									$elm$core$Maybe$Just(
										$author$project$Main$KeeperWantsToIgnorePlayer(playerA)))
								])),
							A2(
							$rtfeldman$elm_css$Html$Styled$div,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$css(
									_List_fromArray(
										[
											$rtfeldman$elm_css$Css$width(
											$rtfeldman$elm_css$Css$pct(20)),
											$rtfeldman$elm_css$Css$textAlign($rtfeldman$elm_css$Css$center)
										]))
								]),
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$text('')
								])),
							A2(
							$rtfeldman$elm_css$Html$Styled$div,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$css(
									_List_fromArray(
										[
											$rtfeldman$elm_css$Css$width(
											$rtfeldman$elm_css$Css$pct(40)),
											$rtfeldman$elm_css$Css$textAlign($rtfeldman$elm_css$Css$center),
											A2(
											$rtfeldman$elm_css$Css$Media$withMedia,
											_List_fromArray(
												[
													A2(
													$rtfeldman$elm_css$Css$Media$only,
													$rtfeldman$elm_css$Css$Media$screen,
													_List_fromArray(
														[
															$rtfeldman$elm_css$Css$Media$maxWidth(
															$rtfeldman$elm_css$Css$px(640))
														]))
												]),
											_List_fromArray(
												[
													$rtfeldman$elm_css$Css$display($rtfeldman$elm_css$Css$none)
												]))
										]))
								]),
							A2($author$project$Main$isPlayerLocallyIgnored, playerB, model) ? _List_fromArray(
								[
									$author$project$Main$zzzUnignoreButton(
									$elm$core$Maybe$Just(
										$author$project$Main$KeeperWantsToUnignorePlayer(playerB)))
								]) : _List_fromArray(
								[
									$author$project$Main$zzzIgnoreButton(
									$elm$core$Maybe$Just(
										$author$project$Main$KeeperWantsToIgnorePlayer(playerB)))
								]))
						])),
					A2(
					$rtfeldman$elm_css$Html$Styled$div,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$css(
							_List_fromArray(
								[
									$rtfeldman$elm_css$Css$marginTop(
									$rtfeldman$elm_css$Css$px(12)),
									$rtfeldman$elm_css$Css$display($rtfeldman$elm_css$Css$none),
									A2(
									$rtfeldman$elm_css$Css$Media$withMedia,
									_List_fromArray(
										[
											A2(
											$rtfeldman$elm_css$Css$Media$only,
											$rtfeldman$elm_css$Css$Media$screen,
											_List_fromArray(
												[
													$rtfeldman$elm_css$Css$Media$maxWidth(
													$rtfeldman$elm_css$Css$px(640))
												]))
										]),
									_List_fromArray(
										[
											$rtfeldman$elm_css$Css$display($rtfeldman$elm_css$Css$block)
										]))
								]))
						]),
					_List_fromArray(
						[
							model.showCustomMatchup ? $author$project$Main$customMatchupUI(model) : $rtfeldman$elm_css$Html$Styled$text('')
						])),
					A2(
					$rtfeldman$elm_css$Html$Styled$div,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$css(
							_List_fromArray(
								[
									A4(
									$rtfeldman$elm_css$Css$padding4,
									$rtfeldman$elm_css$Css$px(32),
									$rtfeldman$elm_css$Css$pct(20),
									$rtfeldman$elm_css$Css$zero,
									$rtfeldman$elm_css$Css$pct(20)),
									A2(
									$rtfeldman$elm_css$Css$Media$withMedia,
									_List_fromArray(
										[
											A2(
											$rtfeldman$elm_css$Css$Media$only,
											$rtfeldman$elm_css$Css$Media$screen,
											_List_fromArray(
												[
													$rtfeldman$elm_css$Css$Media$maxWidth(
													$rtfeldman$elm_css$Css$px(640))
												]))
										]),
									_List_fromArray(
										[
											$rtfeldman$elm_css$Css$display($rtfeldman$elm_css$Css$none)
										]))
								]))
						]),
					_List_fromArray(
						[
							A2(
							$rtfeldman$elm_css$Html$Styled$div,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$css(
									_List_fromArray(
										[
											$rtfeldman$elm_css$Css$displayFlex,
											$rtfeldman$elm_css$Css$justifyContent($rtfeldman$elm_css$Css$center),
											$rtfeldman$elm_css$Css$marginBottom(
											$rtfeldman$elm_css$Css$px(12))
										]))
								]),
							_List_fromArray(
								[
									A2(
									$author$project$Main$blueButton,
									'UNDO',
									A2(
										$elm$core$Maybe$map,
										function (_v2) {
											return $author$project$Main$KeeperWantsToUndo;
										},
										$author$project$History$peekBack(model.history))),
									A2(
									$author$project$Main$blueButton,
									'REDO',
									A2(
										$elm$core$Maybe$map,
										function (_v3) {
											return $author$project$Main$KeeperWantsToRedo;
										},
										$author$project$History$peekForward(model.history))),
									A3(
									$author$project$Main$button,
									$rtfeldman$elm_css$Css$hex('999'),
									'SKIP',
									$elm$core$Maybe$Just($author$project$Main$KeeperWantsToSkipMatch))
								])),
							A2(
							$rtfeldman$elm_css$Html$Styled$div,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$css(
									_List_fromArray(
										[
											$rtfeldman$elm_css$Css$displayFlex,
											$rtfeldman$elm_css$Css$justifyContent($rtfeldman$elm_css$Css$center),
											$rtfeldman$elm_css$Css$marginBottom(
											$rtfeldman$elm_css$Css$px(8))
										]))
								]),
							_List_fromArray(
								[
									A2(
									$author$project$Main$greenButton,
									'CUSTOM MATCHUP',
									$elm$core$Maybe$Just($author$project$Main$KeeperWantsToShowCustomMatchup)),
									A2(
									$author$project$Main$greenButton,
									'ADD PLAYER',
									$elm$core$Maybe$Just($author$project$Main$KeeperWantsToAddNewPlayer))
								])),
							model.showCustomMatchup ? $author$project$Main$customMatchupUI(model) : $rtfeldman$elm_css$Html$Styled$text('')
						]))
				]));
	}
};
var $author$project$Main$SetTimeFilter = function (a) {
	return {$: 'SetTimeFilter', a: a};
};
var $author$project$Main$toggleBtn = F3(
	function (isOn, label, maybeMsg) {
		return A2(
			$rtfeldman$elm_css$Html$Styled$button,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$css(
					_List_fromArray(
						[
							A2(
							$rtfeldman$elm_css$Css$padding2,
							$rtfeldman$elm_css$Css$px(6),
							$rtfeldman$elm_css$Css$px(12)),
							A2(
							$rtfeldman$elm_css$Css$margin2,
							$rtfeldman$elm_css$Css$zero,
							$rtfeldman$elm_css$Css$px(6)),
							$rtfeldman$elm_css$Css$borderRadius(
							$rtfeldman$elm_css$Css$px(9999)),
							$rtfeldman$elm_css$Css$backgroundColor(
							$rtfeldman$elm_css$Css$hex(
								isOn ? '3B82F6' : '6B7280')),
							$rtfeldman$elm_css$Css$color(
							$rtfeldman$elm_css$Css$hex('FFF')),
							$rtfeldman$elm_css$Css$border($rtfeldman$elm_css$Css$zero),
							$rtfeldman$elm_css$Css$cursor($rtfeldman$elm_css$Css$pointer),
							$author$project$Main$modernSansSerif
						])),
					function () {
					if (maybeMsg.$ === 'Just') {
						var m = maybeMsg.a;
						return $rtfeldman$elm_css$Html$Styled$Events$onClick(m);
					} else {
						return $rtfeldman$elm_css$Html$Styled$Attributes$disabled(true);
					}
				}()
				]),
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$text(label)
				]));
	});
var $author$project$Main$filterBar = function (model) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$div,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$Attributes$css(
				_List_fromArray(
					[
						$rtfeldman$elm_css$Css$width(
						$rtfeldman$elm_css$Css$pct(80)),
						A2($rtfeldman$elm_css$Css$margin2, $rtfeldman$elm_css$Css$zero, $rtfeldman$elm_css$Css$auto),
						$rtfeldman$elm_css$Css$marginTop(
						$rtfeldman$elm_css$Css$px(10)),
						$rtfeldman$elm_css$Css$marginBottom(
						$rtfeldman$elm_css$Css$px(10)),
						$rtfeldman$elm_css$Css$textAlign($rtfeldman$elm_css$Css$center)
					]))
			]),
		_List_fromArray(
			[
				A2(
				$rtfeldman$elm_css$Html$Styled$span,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Attributes$css(
						_List_fromArray(
							[
								$rtfeldman$elm_css$Css$marginRight(
								$rtfeldman$elm_css$Css$px(10)),
								$author$project$Main$modernSansSerif,
								$rtfeldman$elm_css$Css$fontWeight(
								$rtfeldman$elm_css$Css$int(600))
							]))
					]),
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$text('Filter:')
					])),
				A3(
				$author$project$Main$toggleBtn,
				_Utils_eq(model.timeFilter, $author$project$Main$All),
				'All',
				$elm$core$Maybe$Just(
					$author$project$Main$SetTimeFilter($author$project$Main$All))),
				A3(
				$author$project$Main$toggleBtn,
				_Utils_eq(model.timeFilter, $author$project$Main$AMOnly),
				'AM',
				$elm$core$Maybe$Just(
					$author$project$Main$SetTimeFilter($author$project$Main$AMOnly))),
				A3(
				$author$project$Main$toggleBtn,
				_Utils_eq(model.timeFilter, $author$project$Main$PMOnly),
				'PM',
				$elm$core$Maybe$Just(
					$author$project$Main$SetTimeFilter($author$project$Main$PMOnly)))
			]));
};
var $rtfeldman$elm_css$Css$fixed = {backgroundAttachment: $rtfeldman$elm_css$Css$Structure$Compatible, position: $rtfeldman$elm_css$Css$Structure$Compatible, tableLayout: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'fixed'};
var $rtfeldman$elm_css$Html$Styled$main_ = $rtfeldman$elm_css$Html$Styled$node('main');
var $rtfeldman$elm_css$Css$Global$a = $rtfeldman$elm_css$Css$Global$typeSelector('a');
var $rtfeldman$elm_css$Css$Global$article = $rtfeldman$elm_css$Css$Global$typeSelector('article');
var $rtfeldman$elm_css$Css$Global$aside = $rtfeldman$elm_css$Css$Global$typeSelector('aside');
var $rtfeldman$elm_css$Css$Global$audio = $rtfeldman$elm_css$Css$Global$typeSelector('audio');
var $rtfeldman$elm_css$Css$baseline = $rtfeldman$elm_css$Css$prop1('baseline');
var $rtfeldman$elm_css$Css$Global$blockquote = $rtfeldman$elm_css$Css$Global$typeSelector('blockquote');
var $rtfeldman$elm_css$Css$Global$body = $rtfeldman$elm_css$Css$Global$typeSelector('body');
var $rtfeldman$elm_css$Css$borderCollapse = $rtfeldman$elm_css$Css$prop1('border-collapse');
var $rtfeldman$elm_css$Css$borderSpacing = $rtfeldman$elm_css$Css$prop1('border-spacing');
var $rtfeldman$elm_css$Css$Global$canvas = $rtfeldman$elm_css$Css$Global$typeSelector('canvas');
var $rtfeldman$elm_css$Css$Global$caption = $rtfeldman$elm_css$Css$Global$typeSelector('caption');
var $rtfeldman$elm_css$Css$Global$code = $rtfeldman$elm_css$Css$Global$typeSelector('code');
var $rtfeldman$elm_css$Css$collapse = {borderCollapse: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'collapse', visibility: $rtfeldman$elm_css$Css$Structure$Compatible};
var $rtfeldman$elm_css$Css$Global$dd = $rtfeldman$elm_css$Css$Global$typeSelector('dd');
var $rtfeldman$elm_css$Css$Global$details = $rtfeldman$elm_css$Css$Global$typeSelector('details');
var $rtfeldman$elm_css$Css$Global$div = $rtfeldman$elm_css$Css$Global$typeSelector('div');
var $rtfeldman$elm_css$Css$Global$dl = $rtfeldman$elm_css$Css$Global$typeSelector('dl');
var $rtfeldman$elm_css$Css$Global$dt = $rtfeldman$elm_css$Css$Global$typeSelector('dt');
var $rtfeldman$elm_css$Css$Global$collectSelectors = function (declarations) {
	collectSelectors:
	while (true) {
		if (!declarations.b) {
			return _List_Nil;
		} else {
			if (declarations.a.$ === 'StyleBlockDeclaration') {
				var _v5 = declarations.a.a;
				var firstSelector = _v5.a;
				var otherSelectors = _v5.b;
				var styles = _v5.c;
				var rest = declarations.b;
				return _Utils_ap(
					A2(
						$elm$core$List$cons,
						A2($rtfeldman$elm_css$Css$Global$unwrapSelector, firstSelector, styles),
						otherSelectors),
					$rtfeldman$elm_css$Css$Global$collectSelectors(rest));
			} else {
				var rest = declarations.b;
				var $temp$declarations = rest;
				declarations = $temp$declarations;
				continue collectSelectors;
			}
		}
	}
};
var $rtfeldman$elm_css$Css$Global$unwrapSelector = F2(
	function (_v0, styles) {
		var sequence = _v0.a;
		var combinators = _v0.b;
		var mPseudo = _v0.c;
		var unwrapSequenceSelector = F2(
			function (style, s) {
				if (style.$ === 'ExtendSelector') {
					var nestedSelector = style.a;
					var evenMoreNestedStyles = style.b;
					return A3(
						$elm$core$List$foldr,
						unwrapSequenceSelector,
						A2($rtfeldman$elm_css$Css$Structure$appendRepeatable, nestedSelector, s),
						evenMoreNestedStyles);
				} else {
					return s;
				}
			});
		var unwrapCombinatorSelector = F2(
			function (style, cs) {
				if (style.$ === 'NestSnippet') {
					var combinator = style.a;
					var snippets = style.b;
					return A2(
						$elm$core$List$append,
						cs,
						A2(
							$elm$core$List$map,
							function (_v3) {
								var s = _v3.a;
								return _Utils_Tuple2(combinator, s);
							},
							A2(
								$elm$core$List$concatMap,
								A2($elm$core$Basics$composeR, $rtfeldman$elm_css$Css$Preprocess$unwrapSnippet, $rtfeldman$elm_css$Css$Global$collectSelectors),
								snippets)));
				} else {
					return cs;
				}
			});
		return A3(
			$rtfeldman$elm_css$Css$Structure$Selector,
			A3($elm$core$List$foldr, unwrapSequenceSelector, sequence, styles),
			A3($elm$core$List$foldr, unwrapCombinatorSelector, combinators, styles),
			mPseudo);
	});
var $rtfeldman$elm_css$Css$Global$each = F2(
	function (snippetCreators, styles) {
		var selectorsToSnippet = function (selectors) {
			if (!selectors.b) {
				return $rtfeldman$elm_css$Css$Preprocess$Snippet(_List_Nil);
			} else {
				var first = selectors.a;
				var rest = selectors.b;
				return $rtfeldman$elm_css$Css$Preprocess$Snippet(
					_List_fromArray(
						[
							$rtfeldman$elm_css$Css$Preprocess$StyleBlockDeclaration(
							A3($rtfeldman$elm_css$Css$Preprocess$StyleBlock, first, rest, styles))
						]));
			}
		};
		return selectorsToSnippet(
			$rtfeldman$elm_css$Css$Global$collectSelectors(
				A2(
					$elm$core$List$concatMap,
					$rtfeldman$elm_css$Css$Preprocess$unwrapSnippet,
					A2(
						$elm$core$List$map,
						$elm$core$Basics$apR(_List_Nil),
						snippetCreators))));
	});
var $rtfeldman$elm_css$Css$Global$fieldset = $rtfeldman$elm_css$Css$Global$typeSelector('fieldset');
var $rtfeldman$elm_css$Css$Global$footer = $rtfeldman$elm_css$Css$Global$typeSelector('footer');
var $rtfeldman$elm_css$Css$Global$form = $rtfeldman$elm_css$Css$Global$typeSelector('form');
var $rtfeldman$elm_css$Css$Global$h1 = $rtfeldman$elm_css$Css$Global$typeSelector('h1');
var $rtfeldman$elm_css$Css$Global$h2 = $rtfeldman$elm_css$Css$Global$typeSelector('h2');
var $rtfeldman$elm_css$Css$Global$h3 = $rtfeldman$elm_css$Css$Global$typeSelector('h3');
var $rtfeldman$elm_css$Css$Global$h4 = $rtfeldman$elm_css$Css$Global$typeSelector('h4');
var $rtfeldman$elm_css$Css$Global$h5 = $rtfeldman$elm_css$Css$Global$typeSelector('h5');
var $rtfeldman$elm_css$Css$Global$h6 = $rtfeldman$elm_css$Css$Global$typeSelector('h6');
var $rtfeldman$elm_css$Css$Global$header = $rtfeldman$elm_css$Css$Global$typeSelector('header');
var $rtfeldman$elm_css$Css$Global$i = $rtfeldman$elm_css$Css$Global$typeSelector('i');
var $rtfeldman$elm_css$Css$Global$img = $rtfeldman$elm_css$Css$Global$typeSelector('img');
var $rtfeldman$elm_css$Css$Global$label = $rtfeldman$elm_css$Css$Global$typeSelector('label');
var $rtfeldman$elm_css$Css$Global$legend = $rtfeldman$elm_css$Css$Global$typeSelector('legend');
var $rtfeldman$elm_css$Css$Global$li = $rtfeldman$elm_css$Css$Global$typeSelector('li');
var $rtfeldman$elm_css$Css$listStyle = $rtfeldman$elm_css$Css$prop1('list-style');
var $rtfeldman$elm_css$Css$Global$menu = $rtfeldman$elm_css$Css$Global$typeSelector('menu');
var $rtfeldman$elm_css$Css$Global$nav = $rtfeldman$elm_css$Css$Global$typeSelector('nav');
var $rtfeldman$elm_css$Css$Global$ol = $rtfeldman$elm_css$Css$Global$typeSelector('ol');
var $rtfeldman$elm_css$Css$Global$p = $rtfeldman$elm_css$Css$Global$typeSelector('p');
var $rtfeldman$elm_css$Css$Global$q = $rtfeldman$elm_css$Css$Global$typeSelector('q');
var $rtfeldman$elm_css$Css$Global$section = $rtfeldman$elm_css$Css$Global$typeSelector('section');
var $rtfeldman$elm_css$Css$Global$selector = F2(
	function (selectorStr, styles) {
		return A2(
			$rtfeldman$elm_css$VirtualDom$Styled$makeSnippet,
			styles,
			A2($rtfeldman$elm_css$Css$Structure$CustomSelector, selectorStr, _List_Nil));
	});
var $rtfeldman$elm_css$Css$Global$span = $rtfeldman$elm_css$Css$Global$typeSelector('span');
var $rtfeldman$elm_css$Css$Global$strong = $rtfeldman$elm_css$Css$Global$typeSelector('strong');
var $rtfeldman$elm_css$Css$Global$summary = $rtfeldman$elm_css$Css$Global$typeSelector('summary');
var $rtfeldman$elm_css$Css$Global$tbody = $rtfeldman$elm_css$Css$Global$typeSelector('tbody');
var $rtfeldman$elm_css$Css$Global$td = $rtfeldman$elm_css$Css$Global$typeSelector('td');
var $rtfeldman$elm_css$Css$Global$tfoot = $rtfeldman$elm_css$Css$Global$typeSelector('tfoot');
var $rtfeldman$elm_css$Css$Global$th = $rtfeldman$elm_css$Css$Global$typeSelector('th');
var $rtfeldman$elm_css$Css$Global$thead = $rtfeldman$elm_css$Css$Global$typeSelector('thead');
var $rtfeldman$elm_css$Css$Global$time = $rtfeldman$elm_css$Css$Global$typeSelector('time');
var $rtfeldman$elm_css$Css$Global$tr = $rtfeldman$elm_css$Css$Global$typeSelector('tr');
var $rtfeldman$elm_css$Css$Global$ul = $rtfeldman$elm_css$Css$Global$typeSelector('ul');
var $rtfeldman$elm_css$Css$verticalAlign = function (fn) {
	return A3(
		$rtfeldman$elm_css$Css$Internal$getOverloadedProperty,
		'verticalAlign',
		'vertical-align',
		fn($rtfeldman$elm_css$Css$Internal$lengthForOverloadedProperty));
};
var $rtfeldman$elm_css$Css$Global$video = $rtfeldman$elm_css$Css$Global$typeSelector('video');
var $BrianHicks$elm_css_reset$Css$Reset$meyerV2 = $rtfeldman$elm_css$Css$Global$global(
	_List_fromArray(
		[
			A2(
			$rtfeldman$elm_css$Css$Global$each,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Css$Global$html,
					$rtfeldman$elm_css$Css$Global$body,
					$rtfeldman$elm_css$Css$Global$div,
					$rtfeldman$elm_css$Css$Global$span,
					$rtfeldman$elm_css$Css$Global$selector('applet'),
					$rtfeldman$elm_css$Css$Global$selector('object'),
					$rtfeldman$elm_css$Css$Global$selector('iframe'),
					$rtfeldman$elm_css$Css$Global$h1,
					$rtfeldman$elm_css$Css$Global$h2,
					$rtfeldman$elm_css$Css$Global$h3,
					$rtfeldman$elm_css$Css$Global$h4,
					$rtfeldman$elm_css$Css$Global$h5,
					$rtfeldman$elm_css$Css$Global$h6,
					$rtfeldman$elm_css$Css$Global$p,
					$rtfeldman$elm_css$Css$Global$blockquote,
					$rtfeldman$elm_css$Css$Global$selector('pre'),
					$rtfeldman$elm_css$Css$Global$a,
					$rtfeldman$elm_css$Css$Global$selector('abbr'),
					$rtfeldman$elm_css$Css$Global$selector('acronym'),
					$rtfeldman$elm_css$Css$Global$selector('address'),
					$rtfeldman$elm_css$Css$Global$selector('big'),
					$rtfeldman$elm_css$Css$Global$selector('cite'),
					$rtfeldman$elm_css$Css$Global$code,
					$rtfeldman$elm_css$Css$Global$selector('del'),
					$rtfeldman$elm_css$Css$Global$selector('dfn'),
					$rtfeldman$elm_css$Css$Global$selector('em'),
					$rtfeldman$elm_css$Css$Global$img,
					$rtfeldman$elm_css$Css$Global$selector('ins'),
					$rtfeldman$elm_css$Css$Global$selector('kbd'),
					$rtfeldman$elm_css$Css$Global$q,
					$rtfeldman$elm_css$Css$Global$selector('s'),
					$rtfeldman$elm_css$Css$Global$selector('samp'),
					$rtfeldman$elm_css$Css$Global$selector('small'),
					$rtfeldman$elm_css$Css$Global$selector('strike'),
					$rtfeldman$elm_css$Css$Global$strong,
					$rtfeldman$elm_css$Css$Global$selector('sub'),
					$rtfeldman$elm_css$Css$Global$selector('sup'),
					$rtfeldman$elm_css$Css$Global$selector('tt'),
					$rtfeldman$elm_css$Css$Global$selector('var'),
					$rtfeldman$elm_css$Css$Global$selector('b'),
					$rtfeldman$elm_css$Css$Global$selector('u'),
					$rtfeldman$elm_css$Css$Global$i,
					$rtfeldman$elm_css$Css$Global$selector('center'),
					$rtfeldman$elm_css$Css$Global$dl,
					$rtfeldman$elm_css$Css$Global$dt,
					$rtfeldman$elm_css$Css$Global$dd,
					$rtfeldman$elm_css$Css$Global$ol,
					$rtfeldman$elm_css$Css$Global$ul,
					$rtfeldman$elm_css$Css$Global$li,
					$rtfeldman$elm_css$Css$Global$fieldset,
					$rtfeldman$elm_css$Css$Global$form,
					$rtfeldman$elm_css$Css$Global$label,
					$rtfeldman$elm_css$Css$Global$legend,
					$rtfeldman$elm_css$Css$Global$selector('table'),
					$rtfeldman$elm_css$Css$Global$caption,
					$rtfeldman$elm_css$Css$Global$tbody,
					$rtfeldman$elm_css$Css$Global$tfoot,
					$rtfeldman$elm_css$Css$Global$thead,
					$rtfeldman$elm_css$Css$Global$tr,
					$rtfeldman$elm_css$Css$Global$th,
					$rtfeldman$elm_css$Css$Global$td,
					$rtfeldman$elm_css$Css$Global$article,
					$rtfeldman$elm_css$Css$Global$aside,
					$rtfeldman$elm_css$Css$Global$canvas,
					$rtfeldman$elm_css$Css$Global$details,
					$rtfeldman$elm_css$Css$Global$selector('embed'),
					$rtfeldman$elm_css$Css$Global$selector('figure'),
					$rtfeldman$elm_css$Css$Global$selector('figcaption'),
					$rtfeldman$elm_css$Css$Global$footer,
					$rtfeldman$elm_css$Css$Global$header,
					$rtfeldman$elm_css$Css$Global$selector('hgroup'),
					$rtfeldman$elm_css$Css$Global$menu,
					$rtfeldman$elm_css$Css$Global$nav,
					$rtfeldman$elm_css$Css$Global$selector('output'),
					$rtfeldman$elm_css$Css$Global$selector('ruby'),
					$rtfeldman$elm_css$Css$Global$section,
					$rtfeldman$elm_css$Css$Global$summary,
					$rtfeldman$elm_css$Css$Global$time,
					$rtfeldman$elm_css$Css$Global$selector('mark'),
					$rtfeldman$elm_css$Css$Global$audio,
					$rtfeldman$elm_css$Css$Global$video
				]),
			_List_fromArray(
				[
					$rtfeldman$elm_css$Css$margin($rtfeldman$elm_css$Css$zero),
					$rtfeldman$elm_css$Css$padding($rtfeldman$elm_css$Css$zero),
					$rtfeldman$elm_css$Css$border($rtfeldman$elm_css$Css$zero),
					$rtfeldman$elm_css$Css$fontSize(
					$rtfeldman$elm_css$Css$pct(100)),
					$rtfeldman$elm_css$Css$verticalAlign($rtfeldman$elm_css$Css$baseline),
					A2($rtfeldman$elm_css$Css$property, 'font', 'inherit')
				])),
			A2(
			$rtfeldman$elm_css$Css$Global$each,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Css$Global$article,
					$rtfeldman$elm_css$Css$Global$aside,
					$rtfeldman$elm_css$Css$Global$details,
					$rtfeldman$elm_css$Css$Global$selector('figcaption'),
					$rtfeldman$elm_css$Css$Global$selector('figure'),
					$rtfeldman$elm_css$Css$Global$footer,
					$rtfeldman$elm_css$Css$Global$header,
					$rtfeldman$elm_css$Css$Global$selector('hgroup'),
					$rtfeldman$elm_css$Css$Global$menu,
					$rtfeldman$elm_css$Css$Global$nav,
					$rtfeldman$elm_css$Css$Global$section
				]),
			_List_fromArray(
				[
					$rtfeldman$elm_css$Css$display($rtfeldman$elm_css$Css$block)
				])),
			$rtfeldman$elm_css$Css$Global$body(
			_List_fromArray(
				[
					$rtfeldman$elm_css$Css$lineHeight(
					$rtfeldman$elm_css$Css$int(1))
				])),
			A2(
			$rtfeldman$elm_css$Css$Global$each,
			_List_fromArray(
				[$rtfeldman$elm_css$Css$Global$ol, $rtfeldman$elm_css$Css$Global$ul]),
			_List_fromArray(
				[
					$rtfeldman$elm_css$Css$listStyle($rtfeldman$elm_css$Css$none)
				])),
			A2(
			$rtfeldman$elm_css$Css$Global$each,
			_List_fromArray(
				[$rtfeldman$elm_css$Css$Global$blockquote, $rtfeldman$elm_css$Css$Global$q]),
			_List_fromArray(
				[
					A2($rtfeldman$elm_css$Css$property, 'quotes', 'none')
				])),
			A2(
			$rtfeldman$elm_css$Css$Global$each,
			_List_fromArray(
				[$rtfeldman$elm_css$Css$Global$blockquote, $rtfeldman$elm_css$Css$Global$q]),
			_List_fromArray(
				[
					$rtfeldman$elm_css$Css$before(
					_List_fromArray(
						[
							A2($rtfeldman$elm_css$Css$property, 'content', '\'\''),
							A2($rtfeldman$elm_css$Css$property, 'content', 'none')
						])),
					$rtfeldman$elm_css$Css$after(
					_List_fromArray(
						[
							A2($rtfeldman$elm_css$Css$property, 'content', '\'\''),
							A2($rtfeldman$elm_css$Css$property, 'content', 'none')
						]))
				])),
			A2(
			$rtfeldman$elm_css$Css$Global$selector,
			'table',
			_List_fromArray(
				[
					$rtfeldman$elm_css$Css$borderCollapse($rtfeldman$elm_css$Css$collapse),
					$rtfeldman$elm_css$Css$borderSpacing($rtfeldman$elm_css$Css$zero)
				]))
		]));
var $rtfeldman$elm_css$Css$notAllowed = {cursor: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'not-allowed'};
var $rtfeldman$elm_css$Css$opacity = $rtfeldman$elm_css$Css$prop1('opacity');
var $author$project$Main$KeeperWantsToRetirePlayer = function (a) {
	return {$: 'KeeperWantsToRetirePlayer', a: a};
};
var $author$project$Main$TogglePlayerAM = function (a) {
	return {$: 'TogglePlayerAM', a: a};
};
var $author$project$Main$TogglePlayerPM = function (a) {
	return {$: 'TogglePlayerPM', a: a};
};
var $rtfeldman$elm_css$Css$borderBottom3 = $rtfeldman$elm_css$Css$prop3('border-bottom');
var $rtfeldman$elm_css$Css$borderRight3 = $rtfeldman$elm_css$Css$prop3('border-right');
var $rtfeldman$elm_css$Css$borderRightWidth = $rtfeldman$elm_css$Css$prop1('border-right-width');
var $author$project$Main$circle = function (color) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$div,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$Attributes$css(
				_List_fromArray(
					[
						$rtfeldman$elm_css$Css$width(
						$rtfeldman$elm_css$Css$px(10)),
						$rtfeldman$elm_css$Css$height(
						$rtfeldman$elm_css$Css$px(10)),
						$rtfeldman$elm_css$Css$borderRadius(
						$rtfeldman$elm_css$Css$pct(100)),
						$rtfeldman$elm_css$Css$backgroundColor(color),
						A2($rtfeldman$elm_css$Css$margin2, $rtfeldman$elm_css$Css$zero, $rtfeldman$elm_css$Css$auto)
					]))
			]),
		_List_Nil);
};
var $rtfeldman$elm_css$Css$borderLeft3 = $rtfeldman$elm_css$Css$prop3('border-left');
var $rtfeldman$elm_css$Css$borderTop3 = $rtfeldman$elm_css$Css$prop3('border-top');
var $rtfeldman$elm_css$Css$margin4 = $rtfeldman$elm_css$Css$prop4('margin');
var $author$project$Main$downArrow = function (color) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$div,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$Attributes$css(
				_List_fromArray(
					[
						$rtfeldman$elm_css$Css$width($rtfeldman$elm_css$Css$zero),
						$rtfeldman$elm_css$Css$height($rtfeldman$elm_css$Css$zero),
						A3(
						$rtfeldman$elm_css$Css$borderLeft3,
						$rtfeldman$elm_css$Css$px(5),
						$rtfeldman$elm_css$Css$solid,
						$rtfeldman$elm_css$Css$transparent),
						A3(
						$rtfeldman$elm_css$Css$borderRight3,
						$rtfeldman$elm_css$Css$px(5),
						$rtfeldman$elm_css$Css$solid,
						$rtfeldman$elm_css$Css$transparent),
						A3(
						$rtfeldman$elm_css$Css$borderTop3,
						$rtfeldman$elm_css$Css$px(10),
						$rtfeldman$elm_css$Css$solid,
						color),
						$rtfeldman$elm_css$Css$display($rtfeldman$elm_css$Css$inlineBlock),
						A4(
						$rtfeldman$elm_css$Css$margin4,
						$rtfeldman$elm_css$Css$px(2),
						$rtfeldman$elm_css$Css$px(5),
						$rtfeldman$elm_css$Css$px(2),
						$rtfeldman$elm_css$Css$px(2))
					]))
			]),
		_List_Nil);
};
var $rtfeldman$elm_css$Css$flexStart = $rtfeldman$elm_css$Css$prop1('flex-start');
var $rtfeldman$elm_css$Css$flexWrap = $rtfeldman$elm_css$Css$prop1('flex-wrap');
var $author$project$Player$htmlKey = function (_v0) {
	var player = _v0.a;
	var _v1 = player.id;
	var idInt = _v1.a;
	return $elm$core$String$fromInt(idInt);
};
var $rtfeldman$elm_css$Css$lastChild = $rtfeldman$elm_css$Css$pseudoClass('last-child');
var $rtfeldman$elm_css$Css$middle = $rtfeldman$elm_css$Css$prop1('middle');
var $rtfeldman$elm_css$Css$noWrap = {flexDirectionOrWrap: $rtfeldman$elm_css$Css$Structure$Compatible, flexWrap: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'nowrap', whiteSpace: $rtfeldman$elm_css$Css$Structure$Compatible};
var $rtfeldman$elm_css$VirtualDom$Styled$KeyedNode = F3(
	function (a, b, c) {
		return {$: 'KeyedNode', a: a, b: b, c: c};
	});
var $rtfeldman$elm_css$VirtualDom$Styled$keyedNode = $rtfeldman$elm_css$VirtualDom$Styled$KeyedNode;
var $rtfeldman$elm_css$Html$Styled$Keyed$node = $rtfeldman$elm_css$VirtualDom$Styled$keyedNode;
var $rtfeldman$elm_css$Css$normal = {featureTagValue: $rtfeldman$elm_css$Css$Structure$Compatible, fontStyle: $rtfeldman$elm_css$Css$Structure$Compatible, fontWeight: $rtfeldman$elm_css$Css$Structure$Compatible, overflowWrap: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'normal', whiteSpace: $rtfeldman$elm_css$Css$Structure$Compatible};
var $rtfeldman$elm_css$Css$overflowX = $rtfeldman$elm_css$Css$prop1('overflow-x');
var $rtfeldman$elm_css$Css$whiteSpace = $rtfeldman$elm_css$Css$prop1('white-space');
var $author$project$Main$smallRedXButtonSmall = function (maybeMsg) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$button,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$Attributes$css(
				_List_fromArray(
					[
						A2(
						$rtfeldman$elm_css$Css$padding2,
						$rtfeldman$elm_css$Css$px(2),
						$rtfeldman$elm_css$Css$px(8)),
						A2(
						$rtfeldman$elm_css$Css$margin2,
						$rtfeldman$elm_css$Css$zero,
						$rtfeldman$elm_css$Css$px(4)),
						$rtfeldman$elm_css$Css$width(
						$rtfeldman$elm_css$Css$px(40)),
						$rtfeldman$elm_css$Css$boxSizing($rtfeldman$elm_css$Css$borderBox),
						$rtfeldman$elm_css$Css$textAlign($rtfeldman$elm_css$Css$center),
						function () {
						if (maybeMsg.$ === 'Just') {
							return $rtfeldman$elm_css$Css$backgroundColor(
								$rtfeldman$elm_css$Css$hex('E02020'));
						} else {
							return $rtfeldman$elm_css$Css$backgroundColor(
								$rtfeldman$elm_css$Css$hex('DDD'));
						}
					}(),
						$rtfeldman$elm_css$Css$border($rtfeldman$elm_css$Css$zero),
						$rtfeldman$elm_css$Css$borderRadius(
						$rtfeldman$elm_css$Css$px(9999)),
						$rtfeldman$elm_css$Css$cursor($rtfeldman$elm_css$Css$pointer),
						$rtfeldman$elm_css$Css$fontSize(
						$rtfeldman$elm_css$Css$px(11)),
						$rtfeldman$elm_css$Css$fontWeight(
						$rtfeldman$elm_css$Css$int(700)),
						$rtfeldman$elm_css$Css$color(
						$rtfeldman$elm_css$Css$hex('FFF')),
						$author$project$Main$modernSansSerif,
						$rtfeldman$elm_css$Css$whiteSpace($rtfeldman$elm_css$Css$noWrap),
						A2(
						$rtfeldman$elm_css$Css$Media$withMedia,
						_List_fromArray(
							[
								A2(
								$rtfeldman$elm_css$Css$Media$only,
								$rtfeldman$elm_css$Css$Media$screen,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Css$Media$maxWidth(
										$rtfeldman$elm_css$Css$px(640))
									]))
							]),
						_List_fromArray(
							[
								A2(
								$rtfeldman$elm_css$Css$padding2,
								$rtfeldman$elm_css$Css$px(2),
								$rtfeldman$elm_css$Css$px(6)),
								$rtfeldman$elm_css$Css$marginRight(
								$rtfeldman$elm_css$Css$px(2)),
								$rtfeldman$elm_css$Css$marginLeft(
								$rtfeldman$elm_css$Css$px(2)),
								$rtfeldman$elm_css$Css$width(
								$rtfeldman$elm_css$Css$px(40)),
								$rtfeldman$elm_css$Css$boxSizing($rtfeldman$elm_css$Css$borderBox),
								$rtfeldman$elm_css$Css$fontSize(
								$rtfeldman$elm_css$Css$px(10))
							]))
					])),
				function () {
				if (maybeMsg.$ === 'Just') {
					var m = maybeMsg.a;
					return $rtfeldman$elm_css$Html$Styled$Events$onClick(m);
				} else {
					return $rtfeldman$elm_css$Html$Styled$Attributes$disabled(true);
				}
			}()
			]),
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$text('X')
			]));
};
var $rtfeldman$elm_css$Css$tableLayout = $rtfeldman$elm_css$Css$prop1('table-layout');
var $rtfeldman$elm_css$Html$Styled$td = $rtfeldman$elm_css$Html$Styled$node('td');
var $rtfeldman$elm_css$Html$Styled$th = $rtfeldman$elm_css$Html$Styled$node('th');
var $author$project$Main$toggleChipSmall = F4(
	function (label, isOn, colorOn, msg) {
		return A2(
			$rtfeldman$elm_css$Html$Styled$button,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$css(
					_List_fromArray(
						[
							$rtfeldman$elm_css$Css$display($rtfeldman$elm_css$Css$inlineBlock),
							A2(
							$rtfeldman$elm_css$Css$padding2,
							$rtfeldman$elm_css$Css$px(2),
							$rtfeldman$elm_css$Css$px(8)),
							$rtfeldman$elm_css$Css$borderRadius(
							$rtfeldman$elm_css$Css$px(9999)),
							isOn ? $rtfeldman$elm_css$Css$backgroundColor(colorOn) : $rtfeldman$elm_css$Css$backgroundColor(
							$rtfeldman$elm_css$Css$hex('E5E7EB')),
							isOn ? $rtfeldman$elm_css$Css$color(
							$rtfeldman$elm_css$Css$hex('FFFFFF')) : $rtfeldman$elm_css$Css$color(
							$rtfeldman$elm_css$Css$hex('6B7280')),
							$rtfeldman$elm_css$Css$fontSize(
							$rtfeldman$elm_css$Css$px(11)),
							$rtfeldman$elm_css$Css$fontWeight(
							$rtfeldman$elm_css$Css$int(700)),
							$rtfeldman$elm_css$Css$border($rtfeldman$elm_css$Css$zero),
							$rtfeldman$elm_css$Css$cursor($rtfeldman$elm_css$Css$pointer),
							$author$project$Main$modernSansSerif,
							$rtfeldman$elm_css$Css$whiteSpace($rtfeldman$elm_css$Css$noWrap),
							$rtfeldman$elm_css$Css$textAlign($rtfeldman$elm_css$Css$center),
							$rtfeldman$elm_css$Css$width(
							$rtfeldman$elm_css$Css$px(40)),
							$rtfeldman$elm_css$Css$boxSizing($rtfeldman$elm_css$Css$borderBox),
							A2(
							$rtfeldman$elm_css$Css$Media$withMedia,
							_List_fromArray(
								[
									A2(
									$rtfeldman$elm_css$Css$Media$only,
									$rtfeldman$elm_css$Css$Media$screen,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Css$Media$maxWidth(
											$rtfeldman$elm_css$Css$px(640))
										]))
								]),
							_List_fromArray(
								[
									A2(
									$rtfeldman$elm_css$Css$padding2,
									$rtfeldman$elm_css$Css$px(2),
									$rtfeldman$elm_css$Css$px(5)),
									$rtfeldman$elm_css$Css$fontSize(
									$rtfeldman$elm_css$Css$px(10)),
									$rtfeldman$elm_css$Css$width(
									$rtfeldman$elm_css$Css$px(40)),
									$rtfeldman$elm_css$Css$boxSizing($rtfeldman$elm_css$Css$borderBox)
								]))
						])),
					$rtfeldman$elm_css$Html$Styled$Events$onClick(msg)
				]),
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$text(label)
				]));
	});
var $rtfeldman$elm_css$Html$Styled$tr = $rtfeldman$elm_css$Html$Styled$node('tr');
var $author$project$Main$upArrow = function (color) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$div,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$Attributes$css(
				_List_fromArray(
					[
						$rtfeldman$elm_css$Css$width($rtfeldman$elm_css$Css$zero),
						$rtfeldman$elm_css$Css$height($rtfeldman$elm_css$Css$zero),
						A3(
						$rtfeldman$elm_css$Css$borderLeft3,
						$rtfeldman$elm_css$Css$px(5),
						$rtfeldman$elm_css$Css$solid,
						$rtfeldman$elm_css$Css$transparent),
						A3(
						$rtfeldman$elm_css$Css$borderRight3,
						$rtfeldman$elm_css$Css$px(5),
						$rtfeldman$elm_css$Css$solid,
						$rtfeldman$elm_css$Css$transparent),
						A3(
						$rtfeldman$elm_css$Css$borderBottom3,
						$rtfeldman$elm_css$Css$px(10),
						$rtfeldman$elm_css$Css$solid,
						color),
						$rtfeldman$elm_css$Css$display($rtfeldman$elm_css$Css$inlineBlock),
						A4(
						$rtfeldman$elm_css$Css$margin4,
						$rtfeldman$elm_css$Css$px(2),
						$rtfeldman$elm_css$Css$px(5),
						$rtfeldman$elm_css$Css$px(2),
						$rtfeldman$elm_css$Css$px(2))
					]))
			]),
		_List_Nil);
};
var $rtfeldman$elm_css$Css$visible = {overflow: $rtfeldman$elm_css$Css$Structure$Compatible, pointerEvents: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'visible', visibility: $rtfeldman$elm_css$Css$Structure$Compatible};
var $rtfeldman$elm_css$Css$wrap = {flexDirectionOrWrap: $rtfeldman$elm_css$Css$Structure$Compatible, flexWrap: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'wrap'};
var $author$project$Main$zzzIgnoreButtonSmall = function (maybeMsg) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$button,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$Attributes$css(
				_List_fromArray(
					[
						A2(
						$rtfeldman$elm_css$Css$padding2,
						$rtfeldman$elm_css$Css$px(2),
						$rtfeldman$elm_css$Css$px(8)),
						A2(
						$rtfeldman$elm_css$Css$margin2,
						$rtfeldman$elm_css$Css$zero,
						$rtfeldman$elm_css$Css$px(4)),
						$rtfeldman$elm_css$Css$width(
						$rtfeldman$elm_css$Css$px(40)),
						$rtfeldman$elm_css$Css$boxSizing($rtfeldman$elm_css$Css$borderBox),
						$rtfeldman$elm_css$Css$textAlign($rtfeldman$elm_css$Css$center),
						$rtfeldman$elm_css$Css$backgroundColor(
						$rtfeldman$elm_css$Css$hex('6B7280')),
						$rtfeldman$elm_css$Css$border($rtfeldman$elm_css$Css$zero),
						$rtfeldman$elm_css$Css$borderRadius(
						$rtfeldman$elm_css$Css$px(9999)),
						$rtfeldman$elm_css$Css$cursor($rtfeldman$elm_css$Css$pointer),
						$rtfeldman$elm_css$Css$fontSize(
						$rtfeldman$elm_css$Css$px(11)),
						$rtfeldman$elm_css$Css$fontWeight(
						$rtfeldman$elm_css$Css$int(700)),
						$rtfeldman$elm_css$Css$color(
						$rtfeldman$elm_css$Css$hex('FFF')),
						$author$project$Main$modernSansSerif,
						$rtfeldman$elm_css$Css$whiteSpace($rtfeldman$elm_css$Css$noWrap),
						A2(
						$rtfeldman$elm_css$Css$Media$withMedia,
						_List_fromArray(
							[
								A2(
								$rtfeldman$elm_css$Css$Media$only,
								$rtfeldman$elm_css$Css$Media$screen,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Css$Media$maxWidth(
										$rtfeldman$elm_css$Css$px(640))
									]))
							]),
						_List_fromArray(
							[
								$rtfeldman$elm_css$Css$paddingLeft(
								$rtfeldman$elm_css$Css$px(6)),
								$rtfeldman$elm_css$Css$paddingRight(
								$rtfeldman$elm_css$Css$px(6)),
								$rtfeldman$elm_css$Css$fontSize(
								$rtfeldman$elm_css$Css$px(10)),
								$rtfeldman$elm_css$Css$width(
								$rtfeldman$elm_css$Css$px(40)),
								$rtfeldman$elm_css$Css$boxSizing($rtfeldman$elm_css$Css$borderBox)
							])),
						A2(
						$rtfeldman$elm_css$Css$Media$withMedia,
						_List_fromArray(
							[
								A2(
								$rtfeldman$elm_css$Css$Media$only,
								$rtfeldman$elm_css$Css$Media$screen,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Css$Media$maxWidth(
										$rtfeldman$elm_css$Css$px(400))
									]))
							]),
						_List_fromArray(
							[
								$rtfeldman$elm_css$Css$paddingLeft(
								$rtfeldman$elm_css$Css$px(4)),
								$rtfeldman$elm_css$Css$paddingRight(
								$rtfeldman$elm_css$Css$px(4)),
								$rtfeldman$elm_css$Css$width(
								$rtfeldman$elm_css$Css$px(40))
							]))
					])),
				function () {
				if (maybeMsg.$ === 'Just') {
					var m = maybeMsg.a;
					return $rtfeldman$elm_css$Html$Styled$Events$onClick(m);
				} else {
					return $rtfeldman$elm_css$Html$Styled$Attributes$disabled(true);
				}
			}()
			]),
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$text('Zzz')
			]));
};
var $author$project$Main$zzzUnignoreButtonSmall = function (maybeMsg) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$button,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$Attributes$css(
				_List_fromArray(
					[
						A2(
						$rtfeldman$elm_css$Css$padding2,
						$rtfeldman$elm_css$Css$px(2),
						$rtfeldman$elm_css$Css$px(8)),
						A2(
						$rtfeldman$elm_css$Css$margin2,
						$rtfeldman$elm_css$Css$zero,
						$rtfeldman$elm_css$Css$px(4)),
						$rtfeldman$elm_css$Css$width(
						$rtfeldman$elm_css$Css$px(40)),
						$rtfeldman$elm_css$Css$boxSizing($rtfeldman$elm_css$Css$borderBox),
						$rtfeldman$elm_css$Css$textAlign($rtfeldman$elm_css$Css$center),
						$rtfeldman$elm_css$Css$backgroundColor(
						$rtfeldman$elm_css$Css$hex('374151')),
						$rtfeldman$elm_css$Css$border($rtfeldman$elm_css$Css$zero),
						$rtfeldman$elm_css$Css$borderRadius(
						$rtfeldman$elm_css$Css$px(9999)),
						$rtfeldman$elm_css$Css$cursor($rtfeldman$elm_css$Css$pointer),
						$rtfeldman$elm_css$Css$fontSize(
						$rtfeldman$elm_css$Css$px(11)),
						$rtfeldman$elm_css$Css$fontWeight(
						$rtfeldman$elm_css$Css$int(700)),
						$rtfeldman$elm_css$Css$color(
						$rtfeldman$elm_css$Css$hex('FFF')),
						$rtfeldman$elm_css$Css$textDecoration($rtfeldman$elm_css$Css$lineThrough),
						$author$project$Main$modernSansSerif,
						$rtfeldman$elm_css$Css$whiteSpace($rtfeldman$elm_css$Css$noWrap),
						A2(
						$rtfeldman$elm_css$Css$Media$withMedia,
						_List_fromArray(
							[
								A2(
								$rtfeldman$elm_css$Css$Media$only,
								$rtfeldman$elm_css$Css$Media$screen,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Css$Media$maxWidth(
										$rtfeldman$elm_css$Css$px(640))
									]))
							]),
						_List_fromArray(
							[
								$rtfeldman$elm_css$Css$paddingLeft(
								$rtfeldman$elm_css$Css$px(6)),
								$rtfeldman$elm_css$Css$paddingRight(
								$rtfeldman$elm_css$Css$px(6)),
								$rtfeldman$elm_css$Css$fontSize(
								$rtfeldman$elm_css$Css$px(10)),
								$rtfeldman$elm_css$Css$width(
								$rtfeldman$elm_css$Css$px(40)),
								$rtfeldman$elm_css$Css$boxSizing($rtfeldman$elm_css$Css$borderBox)
							])),
						A2(
						$rtfeldman$elm_css$Css$Media$withMedia,
						_List_fromArray(
							[
								A2(
								$rtfeldman$elm_css$Css$Media$only,
								$rtfeldman$elm_css$Css$Media$screen,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Css$Media$maxWidth(
										$rtfeldman$elm_css$Css$px(400))
									]))
							]),
						_List_fromArray(
							[
								$rtfeldman$elm_css$Css$paddingLeft(
								$rtfeldman$elm_css$Css$px(4)),
								$rtfeldman$elm_css$Css$paddingRight(
								$rtfeldman$elm_css$Css$px(4)),
								$rtfeldman$elm_css$Css$width(
								$rtfeldman$elm_css$Css$px(40))
							]))
					])),
				function () {
				if (maybeMsg.$ === 'Just') {
					var m = maybeMsg.a;
					return $rtfeldman$elm_css$Html$Styled$Events$onClick(m);
				} else {
					return $rtfeldman$elm_css$Html$Styled$Attributes$disabled(true);
				}
			}()
			]),
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$text('Zzz')
			]));
};
var $author$project$Main$rankings = function (model) {
	var textual = $rtfeldman$elm_css$Css$batch(
		_List_fromArray(
			[
				$rtfeldman$elm_css$Css$fontWeight(
				$rtfeldman$elm_css$Css$int(500)),
				$rtfeldman$elm_css$Css$fontSize(
				$rtfeldman$elm_css$Css$px(16)),
				$rtfeldman$elm_css$Css$lineHeight(
				$rtfeldman$elm_css$Css$px(22)),
				$rtfeldman$elm_css$Css$verticalAlign($rtfeldman$elm_css$Css$middle),
				$author$project$Main$modernSansSerif,
				$rtfeldman$elm_css$Css$paddingLeft(
				$rtfeldman$elm_css$Css$px(15)),
				$rtfeldman$elm_css$Css$whiteSpace($rtfeldman$elm_css$Css$normal),
				$rtfeldman$elm_css$Css$overflow($rtfeldman$elm_css$Css$visible)
			]));
	var shrinkWidth = $rtfeldman$elm_css$Css$batch(
		_List_fromArray(
			[
				$rtfeldman$elm_css$Css$paddingLeft(
				$rtfeldman$elm_css$Css$px(15)),
				$rtfeldman$elm_css$Css$paddingRight(
				$rtfeldman$elm_css$Css$px(15)),
				$rtfeldman$elm_css$Css$width(
				$rtfeldman$elm_css$Css$pct(1)),
				$rtfeldman$elm_css$Css$maxWidth(
				$rtfeldman$elm_css$Css$px(80)),
				$rtfeldman$elm_css$Css$whiteSpace($rtfeldman$elm_css$Css$noWrap)
			]));
	var previousStandings = $elm$core$Dict$fromList(
		A2(
			$elm$core$List$indexedMap,
			F2(
				function (rank, player) {
					return _Utils_Tuple2(
						$author$project$Player$name(player),
						rank);
				}),
			A2(
				$elm$core$List$sortBy,
				function (player) {
					return -$author$project$Player$rating(player);
				},
				$author$project$League$players(
					A2(
						$elm$core$Maybe$withDefault,
						$author$project$History$current(model.history),
						$author$project$History$peekBack(model.history))))));
	var numericRank = $rtfeldman$elm_css$Css$batch(
		_List_fromArray(
			[
				$rtfeldman$elm_css$Css$fontWeight(
				$rtfeldman$elm_css$Css$int(700)),
				$rtfeldman$elm_css$Css$fontSize(
				$rtfeldman$elm_css$Css$px(18)),
				$rtfeldman$elm_css$Css$verticalAlign($rtfeldman$elm_css$Css$middle),
				$author$project$Main$modernSansSerif
			]));
	var numericDim = $rtfeldman$elm_css$Css$batch(
		_List_fromArray(
			[
				$rtfeldman$elm_css$Css$fontWeight(
				$rtfeldman$elm_css$Css$int(500)),
				$rtfeldman$elm_css$Css$fontSize(
				$rtfeldman$elm_css$Css$px(12)),
				$rtfeldman$elm_css$Css$verticalAlign($rtfeldman$elm_css$Css$middle),
				$rtfeldman$elm_css$Css$color(
				$rtfeldman$elm_css$Css$hex('6B7280')),
				$author$project$Main$modernSansSerif
			]));
	var left = $rtfeldman$elm_css$Css$textAlign($rtfeldman$elm_css$Css$left);
	var header = $rtfeldman$elm_css$Css$batch(
		_List_fromArray(
			[
				$rtfeldman$elm_css$Css$paddingRight(
				$rtfeldman$elm_css$Css$px(15)),
				$rtfeldman$elm_css$Css$paddingLeft(
				$rtfeldman$elm_css$Css$px(15)),
				$rtfeldman$elm_css$Css$verticalAlign($rtfeldman$elm_css$Css$middle),
				$author$project$Main$modernSansSerif,
				$rtfeldman$elm_css$Css$fontWeight(
				$rtfeldman$elm_css$Css$int(600)),
				A3(
				$rtfeldman$elm_css$Css$borderRight3,
				$rtfeldman$elm_css$Css$px(1),
				$rtfeldman$elm_css$Css$solid,
				$rtfeldman$elm_css$Css$hex('B1BECE')),
				$rtfeldman$elm_css$Css$lastChild(
				_List_fromArray(
					[
						$rtfeldman$elm_css$Css$borderRightWidth($rtfeldman$elm_css$Css$zero)
					]))
			]));
	var center = $rtfeldman$elm_css$Css$textAlign($rtfeldman$elm_css$Css$center);
	return A2(
		$rtfeldman$elm_css$Html$Styled$div,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$Attributes$css(
				_List_fromArray(
					[
						$rtfeldman$elm_css$Css$width(
						$rtfeldman$elm_css$Css$pct(80)),
						A2($rtfeldman$elm_css$Css$margin2, $rtfeldman$elm_css$Css$zero, $rtfeldman$elm_css$Css$auto),
						$rtfeldman$elm_css$Css$overflowX($rtfeldman$elm_css$Css$auto),
						A2(
						$rtfeldman$elm_css$Css$Media$withMedia,
						_List_fromArray(
							[
								A2(
								$rtfeldman$elm_css$Css$Media$only,
								$rtfeldman$elm_css$Css$Media$screen,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Css$Media$maxWidth(
										$rtfeldman$elm_css$Css$px(640))
									]))
							]),
						_List_fromArray(
							[
								$rtfeldman$elm_css$Css$width(
								$rtfeldman$elm_css$Css$pct(100)),
								$rtfeldman$elm_css$Css$overflowX($rtfeldman$elm_css$Css$auto)
							]))
					]))
			]),
		$elm$core$List$singleton(
			A3(
				$rtfeldman$elm_css$Html$Styled$Keyed$node,
				'table',
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Attributes$css(
						_List_fromArray(
							[
								$rtfeldman$elm_css$Css$width(
								$rtfeldman$elm_css$Css$pct(80)),
								A2($rtfeldman$elm_css$Css$margin2, $rtfeldman$elm_css$Css$zero, $rtfeldman$elm_css$Css$auto),
								$rtfeldman$elm_css$Css$borderCollapse($rtfeldman$elm_css$Css$collapse),
								$rtfeldman$elm_css$Css$tableLayout($rtfeldman$elm_css$Css$auto),
								A2(
								$rtfeldman$elm_css$Css$Media$withMedia,
								_List_fromArray(
									[
										A2(
										$rtfeldman$elm_css$Css$Media$only,
										$rtfeldman$elm_css$Css$Media$screen,
										_List_fromArray(
											[
												$rtfeldman$elm_css$Css$Media$maxWidth(
												$rtfeldman$elm_css$Css$px(640))
											]))
									]),
								_List_fromArray(
									[
										$rtfeldman$elm_css$Css$width(
										$rtfeldman$elm_css$Css$pct(100))
									]))
							]))
					]),
				A2(
					$elm$core$List$cons,
					_Utils_Tuple2(
						'players-header',
						A2(
							$rtfeldman$elm_css$Html$Styled$tr,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$css(
									_List_fromArray(
										[
											$rtfeldman$elm_css$Css$height(
											$rtfeldman$elm_css$Css$px(40)),
											A3(
											$rtfeldman$elm_css$Css$borderBottom3,
											$rtfeldman$elm_css$Css$px(2),
											$rtfeldman$elm_css$Css$solid,
											$rtfeldman$elm_css$Css$hex('D1D5DB'))
										]))
								]),
							_List_fromArray(
								[
									A2(
									$rtfeldman$elm_css$Html$Styled$th,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$css(
											_List_fromArray(
												[
													$rtfeldman$elm_css$Css$width(
													$rtfeldman$elm_css$Css$px(24))
												]))
										]),
									_List_Nil),
									A2(
									$rtfeldman$elm_css$Html$Styled$th,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$css(
											_List_fromArray(
												[
													header,
													center,
													$rtfeldman$elm_css$Css$width(
													$rtfeldman$elm_css$Css$px(60)),
													$rtfeldman$elm_css$Css$maxWidth(
													$rtfeldman$elm_css$Css$px(60))
												]))
										]),
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$text('RANK')
										])),
									A2(
									$rtfeldman$elm_css$Html$Styled$th,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$css(
											_List_fromArray(
												[
													header,
													left,
													$rtfeldman$elm_css$Css$width(
													$rtfeldman$elm_css$Css$pct(25)),
													A2(
													$rtfeldman$elm_css$Css$Media$withMedia,
													_List_fromArray(
														[
															A2(
															$rtfeldman$elm_css$Css$Media$only,
															$rtfeldman$elm_css$Css$Media$screen,
															_List_fromArray(
																[
																	$rtfeldman$elm_css$Css$Media$maxWidth(
																	$rtfeldman$elm_css$Css$px(640))
																]))
														]),
													_List_fromArray(
														[
															$rtfeldman$elm_css$Css$width(
															$rtfeldman$elm_css$Css$pct(60))
														]))
												]))
										]),
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$text('NAME')
										])),
									A2(
									$rtfeldman$elm_css$Html$Styled$th,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$css(
											_List_fromArray(
												[
													header,
													center,
													$rtfeldman$elm_css$Css$width(
													$rtfeldman$elm_css$Css$px(80)),
													$rtfeldman$elm_css$Css$maxWidth(
													$rtfeldman$elm_css$Css$px(80)),
													A2(
													$rtfeldman$elm_css$Css$Media$withMedia,
													_List_fromArray(
														[
															A2(
															$rtfeldman$elm_css$Css$Media$only,
															$rtfeldman$elm_css$Css$Media$screen,
															_List_fromArray(
																[
																	$rtfeldman$elm_css$Css$Media$maxWidth(
																	$rtfeldman$elm_css$Css$px(640))
																]))
														]),
													_List_fromArray(
														[
															$rtfeldman$elm_css$Css$display($rtfeldman$elm_css$Css$none)
														]))
												]))
										]),
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$text('RATING')
										])),
									A2(
									$rtfeldman$elm_css$Html$Styled$th,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$css(
											_List_fromArray(
												[
													header,
													center,
													$rtfeldman$elm_css$Css$width(
													$rtfeldman$elm_css$Css$px(80)),
													$rtfeldman$elm_css$Css$maxWidth(
													$rtfeldman$elm_css$Css$px(80)),
													A2(
													$rtfeldman$elm_css$Css$Media$withMedia,
													_List_fromArray(
														[
															A2(
															$rtfeldman$elm_css$Css$Media$only,
															$rtfeldman$elm_css$Css$Media$screen,
															_List_fromArray(
																[
																	$rtfeldman$elm_css$Css$Media$maxWidth(
																	$rtfeldman$elm_css$Css$px(640))
																]))
														]),
													_List_fromArray(
														[
															$rtfeldman$elm_css$Css$display($rtfeldman$elm_css$Css$none)
														]))
												]))
										]),
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$text('MATCHES')
										])),
									A2(
									$rtfeldman$elm_css$Html$Styled$th,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$css(
											_List_fromArray(
												[
													header,
													center,
													$rtfeldman$elm_css$Css$width(
													$rtfeldman$elm_css$Css$pct(35))
												]))
										]),
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$text('ACTIONS')
										]))
								]))),
					A2(
						$elm$core$List$indexedMap,
						F2(
							function (rank, player) {
								var previousRank = A2(
									$elm$core$Maybe$withDefault,
									rank,
									A2(
										$elm$core$Dict$get,
										$author$project$Player$name(player),
										previousStandings));
								var isPlaying = A2(
									$elm$core$Maybe$withDefault,
									false,
									A2(
										$elm$core$Maybe$map,
										function (_v0) {
											var a = _v0.a;
											var b = _v0.b;
											return _Utils_eq(
												$author$project$Player$id(player),
												$author$project$Player$id(a)) || _Utils_eq(
												$author$project$Player$id(player),
												$author$project$Player$id(b));
										},
										$author$project$League$currentMatch(
											$author$project$History$current(model.history))));
								var isIgnored = A2($author$project$Main$isPlayerLocallyIgnored, player, model);
								return _Utils_Tuple2(
									$author$project$Player$htmlKey(player),
									A2(
										$rtfeldman$elm_css$Html$Styled$tr,
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$Attributes$css(
												_List_fromArray(
													[
														$rtfeldman$elm_css$Css$height(
														$rtfeldman$elm_css$Css$px(40)),
														A3(
														$rtfeldman$elm_css$Css$borderBottom3,
														$rtfeldman$elm_css$Css$px(1),
														$rtfeldman$elm_css$Css$solid,
														$rtfeldman$elm_css$Css$hex('E5E7EB')),
														isIgnored ? $rtfeldman$elm_css$Css$batch(
														_List_fromArray(
															[
																$rtfeldman$elm_css$Css$opacity(
																$rtfeldman$elm_css$Css$num(0.5)),
																$rtfeldman$elm_css$Css$backgroundColor(
																$rtfeldman$elm_css$Css$hex('F9F9F9'))
															])) : $rtfeldman$elm_css$Css$batch(_List_Nil)
													]))
											]),
										_List_fromArray(
											[
												A2(
												$rtfeldman$elm_css$Html$Styled$td,
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$Attributes$css(
														_List_fromArray(
															[
																$rtfeldman$elm_css$Css$verticalAlign($rtfeldman$elm_css$Css$middle),
																$rtfeldman$elm_css$Css$textAlign($rtfeldman$elm_css$Css$center),
																$rtfeldman$elm_css$Css$width(
																$rtfeldman$elm_css$Css$px(24))
															]))
													]),
												isPlaying ? _List_fromArray(
													[
														$author$project$Main$circle(
														$rtfeldman$elm_css$Css$hex('EFE700'))
													]) : ((_Utils_cmp(rank, previousRank) < 0) ? _List_fromArray(
													[
														$author$project$Main$upArrow(
														$rtfeldman$elm_css$Css$hex('6DD400')),
														A2(
														$rtfeldman$elm_css$Html$Styled$span,
														_List_fromArray(
															[
																$rtfeldman$elm_css$Html$Styled$Attributes$css(
																_List_fromArray(
																	[
																		$author$project$Main$modernSansSerif,
																		$rtfeldman$elm_css$Css$color(
																		$rtfeldman$elm_css$Css$hex('6DD400')),
																		$rtfeldman$elm_css$Css$fontSize(
																		$rtfeldman$elm_css$Css$px(14))
																	]))
															]),
														_List_fromArray(
															[
																$rtfeldman$elm_css$Html$Styled$text(
																$elm$core$String$fromInt(previousRank - rank))
															]))
													]) : ((_Utils_cmp(rank, previousRank) > 0) ? _List_fromArray(
													[
														$author$project$Main$downArrow(
														$rtfeldman$elm_css$Css$hex('E02020')),
														A2(
														$rtfeldman$elm_css$Html$Styled$span,
														_List_fromArray(
															[
																$rtfeldman$elm_css$Html$Styled$Attributes$css(
																_List_fromArray(
																	[
																		$author$project$Main$modernSansSerif,
																		$rtfeldman$elm_css$Css$color(
																		$rtfeldman$elm_css$Css$hex('E02020')),
																		$rtfeldman$elm_css$Css$fontSize(
																		$rtfeldman$elm_css$Css$px(14))
																	]))
															]),
														_List_fromArray(
															[
																$rtfeldman$elm_css$Html$Styled$text(
																$elm$core$String$fromInt(
																	$elm$core$Basics$abs(previousRank - rank)))
															]))
													]) : _List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$text('')
													])))),
												A2(
												$rtfeldman$elm_css$Html$Styled$td,
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$Attributes$css(
														_List_fromArray(
															[
																numericRank,
																center,
																$rtfeldman$elm_css$Css$width(
																$rtfeldman$elm_css$Css$px(60)),
																$rtfeldman$elm_css$Css$maxWidth(
																$rtfeldman$elm_css$Css$px(60))
															]))
													]),
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$text(
														$elm$core$String$fromInt(rank + 1))
													])),
												A2(
												$rtfeldman$elm_css$Html$Styled$td,
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$Attributes$css(
														_List_fromArray(
															[
																textual,
																left,
																$rtfeldman$elm_css$Css$width(
																$rtfeldman$elm_css$Css$pct(25)),
																A2(
																$rtfeldman$elm_css$Css$Media$withMedia,
																_List_fromArray(
																	[
																		A2(
																		$rtfeldman$elm_css$Css$Media$only,
																		$rtfeldman$elm_css$Css$Media$screen,
																		_List_fromArray(
																			[
																				$rtfeldman$elm_css$Css$Media$maxWidth(
																				$rtfeldman$elm_css$Css$px(640))
																			]))
																	]),
																_List_fromArray(
																	[
																		$rtfeldman$elm_css$Css$width(
																		$rtfeldman$elm_css$Css$pct(60))
																	]))
															]))
													]),
												_List_fromArray(
													[
														A2(
														$rtfeldman$elm_css$Html$Styled$div,
														_List_fromArray(
															[
																$rtfeldman$elm_css$Html$Styled$Attributes$css(
																_List_fromArray(
																	[
																		$rtfeldman$elm_css$Css$displayFlex,
																		$rtfeldman$elm_css$Css$alignItems($rtfeldman$elm_css$Css$center)
																	]))
															]),
														_List_fromArray(
															[
																A2(
																$rtfeldman$elm_css$Html$Styled$span,
																_List_Nil,
																_List_fromArray(
																	[
																		$rtfeldman$elm_css$Html$Styled$text(
																		$author$project$Player$name(player))
																	])),
																isIgnored ? A2(
																$rtfeldman$elm_css$Html$Styled$span,
																_List_fromArray(
																	[
																		$rtfeldman$elm_css$Html$Styled$Attributes$css(
																		_List_fromArray(
																			[
																				$rtfeldman$elm_css$Css$marginLeft(
																				$rtfeldman$elm_css$Css$px(8)),
																				A2(
																				$rtfeldman$elm_css$Css$padding2,
																				$rtfeldman$elm_css$Css$px(2),
																				$rtfeldman$elm_css$Css$px(6)),
																				$rtfeldman$elm_css$Css$backgroundColor(
																				$rtfeldman$elm_css$Css$hex('FEF3C7')),
																				$rtfeldman$elm_css$Css$color(
																				$rtfeldman$elm_css$Css$hex('92400E')),
																				$rtfeldman$elm_css$Css$fontSize(
																				$rtfeldman$elm_css$Css$px(10)),
																				$rtfeldman$elm_css$Css$fontWeight(
																				$rtfeldman$elm_css$Css$int(600)),
																				$rtfeldman$elm_css$Css$borderRadius(
																				$rtfeldman$elm_css$Css$px(4)),
																				$rtfeldman$elm_css$Css$textTransform($rtfeldman$elm_css$Css$uppercase),
																				$author$project$Main$modernSansSerif
																			]))
																	]),
																_List_fromArray(
																	[
																		$rtfeldman$elm_css$Html$Styled$text('SNOOZED')
																	])) : $rtfeldman$elm_css$Html$Styled$text('')
															]))
													])),
												A2(
												$rtfeldman$elm_css$Html$Styled$td,
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$Attributes$css(
														_List_fromArray(
															[
																numericDim,
																center,
																$rtfeldman$elm_css$Css$width(
																$rtfeldman$elm_css$Css$px(80)),
																$rtfeldman$elm_css$Css$maxWidth(
																$rtfeldman$elm_css$Css$px(80)),
																A2(
																$rtfeldman$elm_css$Css$Media$withMedia,
																_List_fromArray(
																	[
																		A2(
																		$rtfeldman$elm_css$Css$Media$only,
																		$rtfeldman$elm_css$Css$Media$screen,
																		_List_fromArray(
																			[
																				$rtfeldman$elm_css$Css$Media$maxWidth(
																				$rtfeldman$elm_css$Css$px(640))
																			]))
																	]),
																_List_fromArray(
																	[
																		$rtfeldman$elm_css$Css$display($rtfeldman$elm_css$Css$none)
																	]))
															]))
													]),
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$text(
														$elm$core$String$fromInt(
															$author$project$Player$rating(player)))
													])),
												A2(
												$rtfeldman$elm_css$Html$Styled$td,
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$Attributes$css(
														_List_fromArray(
															[
																numericDim,
																center,
																$rtfeldman$elm_css$Css$width(
																$rtfeldman$elm_css$Css$px(80)),
																$rtfeldman$elm_css$Css$maxWidth(
																$rtfeldman$elm_css$Css$px(80)),
																A2(
																$rtfeldman$elm_css$Css$Media$withMedia,
																_List_fromArray(
																	[
																		A2(
																		$rtfeldman$elm_css$Css$Media$only,
																		$rtfeldman$elm_css$Css$Media$screen,
																		_List_fromArray(
																			[
																				$rtfeldman$elm_css$Css$Media$maxWidth(
																				$rtfeldman$elm_css$Css$px(640))
																			]))
																	]),
																_List_fromArray(
																	[
																		$rtfeldman$elm_css$Css$display($rtfeldman$elm_css$Css$none)
																	]))
															]))
													]),
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$text(
														$elm$core$String$fromInt(
															$author$project$Player$matchesPlayed(player)))
													])),
												A2(
												$rtfeldman$elm_css$Html$Styled$td,
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$Attributes$css(
														_List_fromArray(
															[
																$rtfeldman$elm_css$Css$verticalAlign($rtfeldman$elm_css$Css$middle),
																$rtfeldman$elm_css$Css$width(
																$rtfeldman$elm_css$Css$pct(35))
															]))
													]),
												_List_fromArray(
													[
														A2(
														$rtfeldman$elm_css$Html$Styled$div,
														_List_fromArray(
															[
																$rtfeldman$elm_css$Html$Styled$Attributes$css(
																_List_fromArray(
																	[
																		$rtfeldman$elm_css$Css$displayFlex,
																		$rtfeldman$elm_css$Css$flexWrap($rtfeldman$elm_css$Css$wrap),
																		$rtfeldman$elm_css$Css$alignItems($rtfeldman$elm_css$Css$center),
																		$rtfeldman$elm_css$Css$justifyContent($rtfeldman$elm_css$Css$center),
																		A2(
																		$rtfeldman$elm_css$Css$Media$withMedia,
																		_List_fromArray(
																			[
																				A2(
																				$rtfeldman$elm_css$Css$Media$only,
																				$rtfeldman$elm_css$Css$Media$screen,
																				_List_fromArray(
																					[
																						$rtfeldman$elm_css$Css$Media$maxWidth(
																						$rtfeldman$elm_css$Css$px(640))
																					]))
																			]),
																		_List_fromArray(
																			[
																				$rtfeldman$elm_css$Css$justifyContent($rtfeldman$elm_css$Css$flexStart)
																			]))
																	]))
															]),
														function () {
															var baseActions = A2($author$project$Main$isPlayerLocallyIgnored, player, model) ? _List_fromArray(
																[
																	$author$project$Main$zzzUnignoreButtonSmall(
																	$elm$core$Maybe$Just(
																		$author$project$Main$KeeperWantsToUnignorePlayer(player)))
																]) : _List_fromArray(
																[
																	$author$project$Main$smallRedXButtonSmall(
																	$elm$core$Maybe$Just(
																		$author$project$Main$KeeperWantsToRetirePlayer(player))),
																	A2(
																	$rtfeldman$elm_css$Html$Styled$span,
																	_List_fromArray(
																		[
																			$rtfeldman$elm_css$Html$Styled$Attributes$css(
																			_List_fromArray(
																				[
																					$rtfeldman$elm_css$Css$paddingLeft(
																					$rtfeldman$elm_css$Css$px(6)),
																					A2(
																					$rtfeldman$elm_css$Css$Media$withMedia,
																					_List_fromArray(
																						[
																							A2(
																							$rtfeldman$elm_css$Css$Media$only,
																							$rtfeldman$elm_css$Css$Media$screen,
																							_List_fromArray(
																								[
																									$rtfeldman$elm_css$Css$Media$maxWidth(
																									$rtfeldman$elm_css$Css$px(640))
																								]))
																						]),
																					_List_fromArray(
																						[
																							$rtfeldman$elm_css$Css$paddingLeft(
																							$rtfeldman$elm_css$Css$px(2))
																						]))
																				]))
																		]),
																	_List_fromArray(
																		[
																			$author$project$Main$zzzIgnoreButtonSmall(
																			$elm$core$Maybe$Just(
																				$author$project$Main$KeeperWantsToIgnorePlayer(player)))
																		]))
																]);
															return _Utils_ap(
																baseActions,
																_List_fromArray(
																	[
																		A2(
																		$rtfeldman$elm_css$Html$Styled$span,
																		_List_fromArray(
																			[
																				$rtfeldman$elm_css$Html$Styled$Attributes$css(
																				_List_fromArray(
																					[
																						$rtfeldman$elm_css$Css$paddingLeft(
																						$rtfeldman$elm_css$Css$px(6)),
																						A2(
																						$rtfeldman$elm_css$Css$Media$withMedia,
																						_List_fromArray(
																							[
																								A2(
																								$rtfeldman$elm_css$Css$Media$only,
																								$rtfeldman$elm_css$Css$Media$screen,
																								_List_fromArray(
																									[
																										$rtfeldman$elm_css$Css$Media$maxWidth(
																										$rtfeldman$elm_css$Css$px(640))
																									]))
																							]),
																						_List_fromArray(
																							[
																								$rtfeldman$elm_css$Css$paddingLeft(
																								$rtfeldman$elm_css$Css$px(2))
																							]))
																					]))
																			]),
																		_List_fromArray(
																			[
																				A4(
																				$author$project$Main$toggleChipSmall,
																				'AM',
																				$author$project$Player$playsAM(player),
																				$rtfeldman$elm_css$Css$hex('F59E0B'),
																				$author$project$Main$TogglePlayerAM(player))
																			])),
																		A2(
																		$rtfeldman$elm_css$Html$Styled$span,
																		_List_fromArray(
																			[
																				$rtfeldman$elm_css$Html$Styled$Attributes$css(
																				_List_fromArray(
																					[
																						$rtfeldman$elm_css$Css$paddingLeft(
																						$rtfeldman$elm_css$Css$px(4)),
																						A2(
																						$rtfeldman$elm_css$Css$Media$withMedia,
																						_List_fromArray(
																							[
																								A2(
																								$rtfeldman$elm_css$Css$Media$only,
																								$rtfeldman$elm_css$Css$Media$screen,
																								_List_fromArray(
																									[
																										$rtfeldman$elm_css$Css$Media$maxWidth(
																										$rtfeldman$elm_css$Css$px(640))
																									]))
																							]),
																						_List_fromArray(
																							[
																								$rtfeldman$elm_css$Css$paddingLeft(
																								$rtfeldman$elm_css$Css$px(2))
																							]))
																					]))
																			]),
																		_List_fromArray(
																			[
																				A4(
																				$author$project$Main$toggleChipSmall,
																				'PM',
																				$author$project$Player$playsPM(player),
																				$rtfeldman$elm_css$Css$hex('8B5CF6'),
																				$author$project$Main$TogglePlayerPM(player))
																			]))
																	]));
														}())
													]))
											])));
							}),
						A2(
							$elm$core$List$sortBy,
							function (player) {
								return -$author$project$Player$rating(player);
							},
							A2(
								$elm$core$List$filter,
								$author$project$Main$timePlayerFilter(model),
								$author$project$League$players(
									$author$project$History$current(model.history)))))))));
};
var $rtfeldman$elm_css$Css$spaceAround = $rtfeldman$elm_css$Css$prop1('space-around');
var $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles = F2(
	function (_v0, styles) {
		var isCssStyles = _v0.b;
		var cssTemplate = _v0.c;
		if (isCssStyles) {
			var _v1 = A2($elm$core$Dict$get, cssTemplate, styles);
			if (_v1.$ === 'Just') {
				return styles;
			} else {
				return A3(
					$elm$core$Dict$insert,
					cssTemplate,
					$rtfeldman$elm_css$Hash$fromString(cssTemplate),
					styles);
			}
		} else {
			return styles;
		}
	});
var $rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute = F2(
	function (styles, _v0) {
		var val = _v0.a;
		var isCssStyles = _v0.b;
		var cssTemplate = _v0.c;
		if (isCssStyles) {
			var _v1 = A2($elm$core$Dict$get, cssTemplate, styles);
			if (_v1.$ === 'Just') {
				var classname = _v1.a;
				return A2(
					$elm$virtual_dom$VirtualDom$property,
					'className',
					$elm$json$Json$Encode$string(classname));
			} else {
				return A2(
					$elm$virtual_dom$VirtualDom$property,
					'className',
					$elm$json$Json$Encode$string('_unstyled'));
			}
		} else {
			return val;
		}
	});
var $rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttributeNS = F2(
	function (styles, _v0) {
		var val = _v0.a;
		var isCssStyles = _v0.b;
		var cssTemplate = _v0.c;
		if (isCssStyles) {
			var _v1 = A2($elm$core$Dict$get, cssTemplate, styles);
			if (_v1.$ === 'Just') {
				var classname = _v1.a;
				return A2($elm$virtual_dom$VirtualDom$attribute, 'class', classname);
			} else {
				return A2($elm$virtual_dom$VirtualDom$attribute, 'class', '_unstyled');
			}
		} else {
			return val;
		}
	});
var $elm$virtual_dom$VirtualDom$keyedNode = function (tag) {
	return _VirtualDom_keyedNode(
		_VirtualDom_noScript(tag));
};
var $elm$virtual_dom$VirtualDom$keyedNodeNS = F2(
	function (namespace, tag) {
		return A2(
			_VirtualDom_keyedNodeNS,
			namespace,
			_VirtualDom_noScript(tag));
	});
var $elm$virtual_dom$VirtualDom$nodeNS = F2(
	function (namespace, tag) {
		return A2(
			_VirtualDom_nodeNS,
			namespace,
			_VirtualDom_noScript(tag));
	});
var $rtfeldman$elm_css$VirtualDom$Styled$accumulateKeyedStyledHtml = F2(
	function (_v6, _v7) {
		var key = _v6.a;
		var html = _v6.b;
		var pairs = _v7.a;
		var styles = _v7.b;
		switch (html.$) {
			case 'Unstyled':
				var vdom = html.a;
				return _Utils_Tuple2(
					A2(
						$elm$core$List$cons,
						_Utils_Tuple2(key, vdom),
						pairs),
					styles);
			case 'Node':
				var elemType = html.a;
				var properties = html.b;
				var children = html.c;
				var combinedStyles = A3($elm$core$List$foldl, $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, styles, properties);
				var _v9 = A3(
					$elm$core$List$foldl,
					$rtfeldman$elm_css$VirtualDom$Styled$accumulateStyledHtml,
					_Utils_Tuple2(_List_Nil, combinedStyles),
					children);
				var childNodes = _v9.a;
				var finalStyles = _v9.b;
				var vdom = A3(
					$elm$virtual_dom$VirtualDom$node,
					elemType,
					A2(
						$elm$core$List$map,
						$rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute(finalStyles),
						properties),
					$elm$core$List$reverse(childNodes));
				return _Utils_Tuple2(
					A2(
						$elm$core$List$cons,
						_Utils_Tuple2(key, vdom),
						pairs),
					finalStyles);
			case 'NodeNS':
				var ns = html.a;
				var elemType = html.b;
				var properties = html.c;
				var children = html.d;
				var combinedStyles = A3($elm$core$List$foldl, $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, styles, properties);
				var _v10 = A3(
					$elm$core$List$foldl,
					$rtfeldman$elm_css$VirtualDom$Styled$accumulateStyledHtml,
					_Utils_Tuple2(_List_Nil, combinedStyles),
					children);
				var childNodes = _v10.a;
				var finalStyles = _v10.b;
				var vdom = A4(
					$elm$virtual_dom$VirtualDom$nodeNS,
					ns,
					elemType,
					A2(
						$elm$core$List$map,
						$rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute(finalStyles),
						properties),
					$elm$core$List$reverse(childNodes));
				return _Utils_Tuple2(
					A2(
						$elm$core$List$cons,
						_Utils_Tuple2(key, vdom),
						pairs),
					finalStyles);
			case 'KeyedNode':
				var elemType = html.a;
				var properties = html.b;
				var children = html.c;
				var combinedStyles = A3($elm$core$List$foldl, $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, styles, properties);
				var _v11 = A3(
					$elm$core$List$foldl,
					$rtfeldman$elm_css$VirtualDom$Styled$accumulateKeyedStyledHtml,
					_Utils_Tuple2(_List_Nil, combinedStyles),
					children);
				var childNodes = _v11.a;
				var finalStyles = _v11.b;
				var vdom = A3(
					$elm$virtual_dom$VirtualDom$keyedNode,
					elemType,
					A2(
						$elm$core$List$map,
						$rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute(finalStyles),
						properties),
					$elm$core$List$reverse(childNodes));
				return _Utils_Tuple2(
					A2(
						$elm$core$List$cons,
						_Utils_Tuple2(key, vdom),
						pairs),
					finalStyles);
			default:
				var ns = html.a;
				var elemType = html.b;
				var properties = html.c;
				var children = html.d;
				var combinedStyles = A3($elm$core$List$foldl, $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, styles, properties);
				var _v12 = A3(
					$elm$core$List$foldl,
					$rtfeldman$elm_css$VirtualDom$Styled$accumulateKeyedStyledHtml,
					_Utils_Tuple2(_List_Nil, combinedStyles),
					children);
				var childNodes = _v12.a;
				var finalStyles = _v12.b;
				var vdom = A4(
					$elm$virtual_dom$VirtualDom$keyedNodeNS,
					ns,
					elemType,
					A2(
						$elm$core$List$map,
						$rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute(finalStyles),
						properties),
					$elm$core$List$reverse(childNodes));
				return _Utils_Tuple2(
					A2(
						$elm$core$List$cons,
						_Utils_Tuple2(key, vdom),
						pairs),
					finalStyles);
		}
	});
var $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyledHtml = F2(
	function (html, _v0) {
		var nodes = _v0.a;
		var styles = _v0.b;
		switch (html.$) {
			case 'Unstyled':
				var vdomNode = html.a;
				return _Utils_Tuple2(
					A2($elm$core$List$cons, vdomNode, nodes),
					styles);
			case 'Node':
				var elemType = html.a;
				var properties = html.b;
				var children = html.c;
				var combinedStyles = A3($elm$core$List$foldl, $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, styles, properties);
				var _v2 = A3(
					$elm$core$List$foldl,
					$rtfeldman$elm_css$VirtualDom$Styled$accumulateStyledHtml,
					_Utils_Tuple2(_List_Nil, combinedStyles),
					children);
				var childNodes = _v2.a;
				var finalStyles = _v2.b;
				var vdomNode = A3(
					$elm$virtual_dom$VirtualDom$node,
					elemType,
					A2(
						$elm$core$List$map,
						$rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute(finalStyles),
						properties),
					$elm$core$List$reverse(childNodes));
				return _Utils_Tuple2(
					A2($elm$core$List$cons, vdomNode, nodes),
					finalStyles);
			case 'NodeNS':
				var ns = html.a;
				var elemType = html.b;
				var properties = html.c;
				var children = html.d;
				var combinedStyles = A3($elm$core$List$foldl, $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, styles, properties);
				var _v3 = A3(
					$elm$core$List$foldl,
					$rtfeldman$elm_css$VirtualDom$Styled$accumulateStyledHtml,
					_Utils_Tuple2(_List_Nil, combinedStyles),
					children);
				var childNodes = _v3.a;
				var finalStyles = _v3.b;
				var vdomNode = A4(
					$elm$virtual_dom$VirtualDom$nodeNS,
					ns,
					elemType,
					A2(
						$elm$core$List$map,
						$rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttributeNS(finalStyles),
						properties),
					$elm$core$List$reverse(childNodes));
				return _Utils_Tuple2(
					A2($elm$core$List$cons, vdomNode, nodes),
					finalStyles);
			case 'KeyedNode':
				var elemType = html.a;
				var properties = html.b;
				var children = html.c;
				var combinedStyles = A3($elm$core$List$foldl, $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, styles, properties);
				var _v4 = A3(
					$elm$core$List$foldl,
					$rtfeldman$elm_css$VirtualDom$Styled$accumulateKeyedStyledHtml,
					_Utils_Tuple2(_List_Nil, combinedStyles),
					children);
				var childNodes = _v4.a;
				var finalStyles = _v4.b;
				var vdomNode = A3(
					$elm$virtual_dom$VirtualDom$keyedNode,
					elemType,
					A2(
						$elm$core$List$map,
						$rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute(finalStyles),
						properties),
					$elm$core$List$reverse(childNodes));
				return _Utils_Tuple2(
					A2($elm$core$List$cons, vdomNode, nodes),
					finalStyles);
			default:
				var ns = html.a;
				var elemType = html.b;
				var properties = html.c;
				var children = html.d;
				var combinedStyles = A3($elm$core$List$foldl, $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, styles, properties);
				var _v5 = A3(
					$elm$core$List$foldl,
					$rtfeldman$elm_css$VirtualDom$Styled$accumulateKeyedStyledHtml,
					_Utils_Tuple2(_List_Nil, combinedStyles),
					children);
				var childNodes = _v5.a;
				var finalStyles = _v5.b;
				var vdomNode = A4(
					$elm$virtual_dom$VirtualDom$keyedNodeNS,
					ns,
					elemType,
					A2(
						$elm$core$List$map,
						$rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttributeNS(finalStyles),
						properties),
					$elm$core$List$reverse(childNodes));
				return _Utils_Tuple2(
					A2($elm$core$List$cons, vdomNode, nodes),
					finalStyles);
		}
	});
var $rtfeldman$elm_css$VirtualDom$Styled$styleToDeclaration = F3(
	function (template, classname, declaration) {
		return declaration + ('\n' + A3($elm$core$String$replace, $rtfeldman$elm_css$VirtualDom$Styled$classnameStandin, classname, template));
	});
var $rtfeldman$elm_css$VirtualDom$Styled$toDeclaration = function (dict) {
	return A3($elm$core$Dict$foldl, $rtfeldman$elm_css$VirtualDom$Styled$styleToDeclaration, '', dict);
};
var $rtfeldman$elm_css$VirtualDom$Styled$toStyleNode = F2(
	function (maybeNonce, styles) {
		return A3(
			$elm$virtual_dom$VirtualDom$node,
			'span',
			_List_fromArray(
				[
					A2($elm$virtual_dom$VirtualDom$attribute, 'style', 'display: none;'),
					A2($elm$virtual_dom$VirtualDom$attribute, 'class', 'elm-css-style-wrapper')
				]),
			_List_fromArray(
				[
					A3(
					$elm$virtual_dom$VirtualDom$node,
					'style',
					function () {
						if (maybeNonce.$ === 'Just') {
							var nonce = maybeNonce.a.a;
							return _List_fromArray(
								[
									A2($elm$virtual_dom$VirtualDom$attribute, 'nonce', nonce)
								]);
						} else {
							return _List_Nil;
						}
					}(),
					$elm$core$List$singleton(
						$elm$virtual_dom$VirtualDom$text(
							$rtfeldman$elm_css$VirtualDom$Styled$toDeclaration(styles))))
				]));
	});
var $rtfeldman$elm_css$VirtualDom$Styled$unstyle = F4(
	function (maybeNonce, elemType, properties, children) {
		var initialStyles = A3($elm$core$List$foldl, $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, $elm$core$Dict$empty, properties);
		var _v0 = A3(
			$elm$core$List$foldl,
			$rtfeldman$elm_css$VirtualDom$Styled$accumulateStyledHtml,
			_Utils_Tuple2(_List_Nil, initialStyles),
			children);
		var childNodes = _v0.a;
		var styles = _v0.b;
		var styleNode = A2($rtfeldman$elm_css$VirtualDom$Styled$toStyleNode, maybeNonce, styles);
		var unstyledProperties = A2(
			$elm$core$List$map,
			$rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute(styles),
			properties);
		return A3(
			$elm$virtual_dom$VirtualDom$node,
			elemType,
			unstyledProperties,
			A2(
				$elm$core$List$cons,
				styleNode,
				$elm$core$List$reverse(childNodes)));
	});
var $rtfeldman$elm_css$VirtualDom$Styled$containsKey = F2(
	function (key, pairs) {
		containsKey:
		while (true) {
			if (!pairs.b) {
				return false;
			} else {
				var _v1 = pairs.a;
				var str = _v1.a;
				var rest = pairs.b;
				if (_Utils_eq(key, str)) {
					return true;
				} else {
					var $temp$key = key,
						$temp$pairs = rest;
					key = $temp$key;
					pairs = $temp$pairs;
					continue containsKey;
				}
			}
		}
	});
var $rtfeldman$elm_css$VirtualDom$Styled$getUnusedKey = F2(
	function (_default, pairs) {
		getUnusedKey:
		while (true) {
			if (!pairs.b) {
				return _default;
			} else {
				var _v1 = pairs.a;
				var firstKey = _v1.a;
				var rest = pairs.b;
				var newKey = '_' + firstKey;
				if (A2($rtfeldman$elm_css$VirtualDom$Styled$containsKey, newKey, rest)) {
					var $temp$default = newKey,
						$temp$pairs = rest;
					_default = $temp$default;
					pairs = $temp$pairs;
					continue getUnusedKey;
				} else {
					return newKey;
				}
			}
		}
	});
var $rtfeldman$elm_css$VirtualDom$Styled$toKeyedStyleNode = F3(
	function (maybeNonce, allStyles, keyedChildNodes) {
		var styleNodeKey = A2($rtfeldman$elm_css$VirtualDom$Styled$getUnusedKey, '_', keyedChildNodes);
		var finalNode = A2($rtfeldman$elm_css$VirtualDom$Styled$toStyleNode, maybeNonce, allStyles);
		return _Utils_Tuple2(styleNodeKey, finalNode);
	});
var $rtfeldman$elm_css$VirtualDom$Styled$unstyleKeyed = F4(
	function (maybeNonce, elemType, properties, keyedChildren) {
		var initialStyles = A3($elm$core$List$foldl, $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, $elm$core$Dict$empty, properties);
		var _v0 = A3(
			$elm$core$List$foldl,
			$rtfeldman$elm_css$VirtualDom$Styled$accumulateKeyedStyledHtml,
			_Utils_Tuple2(_List_Nil, initialStyles),
			keyedChildren);
		var keyedChildNodes = _v0.a;
		var styles = _v0.b;
		var keyedStyleNode = A3($rtfeldman$elm_css$VirtualDom$Styled$toKeyedStyleNode, maybeNonce, styles, keyedChildNodes);
		var unstyledProperties = A2(
			$elm$core$List$map,
			$rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute(styles),
			properties);
		return A3(
			$elm$virtual_dom$VirtualDom$keyedNode,
			elemType,
			unstyledProperties,
			A2(
				$elm$core$List$cons,
				keyedStyleNode,
				$elm$core$List$reverse(keyedChildNodes)));
	});
var $rtfeldman$elm_css$VirtualDom$Styled$unstyleKeyedNS = F5(
	function (maybeNonce, ns, elemType, properties, keyedChildren) {
		var initialStyles = A3($elm$core$List$foldl, $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, $elm$core$Dict$empty, properties);
		var _v0 = A3(
			$elm$core$List$foldl,
			$rtfeldman$elm_css$VirtualDom$Styled$accumulateKeyedStyledHtml,
			_Utils_Tuple2(_List_Nil, initialStyles),
			keyedChildren);
		var keyedChildNodes = _v0.a;
		var styles = _v0.b;
		var keyedStyleNode = A3($rtfeldman$elm_css$VirtualDom$Styled$toKeyedStyleNode, maybeNonce, styles, keyedChildNodes);
		var unstyledProperties = A2(
			$elm$core$List$map,
			$rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttributeNS(styles),
			properties);
		return A4(
			$elm$virtual_dom$VirtualDom$keyedNodeNS,
			ns,
			elemType,
			unstyledProperties,
			A2(
				$elm$core$List$cons,
				keyedStyleNode,
				$elm$core$List$reverse(keyedChildNodes)));
	});
var $rtfeldman$elm_css$VirtualDom$Styled$unstyleNS = F5(
	function (maybeNonce, ns, elemType, properties, children) {
		var initialStyles = A3($elm$core$List$foldl, $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, $elm$core$Dict$empty, properties);
		var _v0 = A3(
			$elm$core$List$foldl,
			$rtfeldman$elm_css$VirtualDom$Styled$accumulateStyledHtml,
			_Utils_Tuple2(_List_Nil, initialStyles),
			children);
		var childNodes = _v0.a;
		var styles = _v0.b;
		var styleNode = A2($rtfeldman$elm_css$VirtualDom$Styled$toStyleNode, maybeNonce, styles);
		var unstyledProperties = A2(
			$elm$core$List$map,
			$rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttributeNS(styles),
			properties);
		return A4(
			$elm$virtual_dom$VirtualDom$nodeNS,
			ns,
			elemType,
			unstyledProperties,
			A2(
				$elm$core$List$cons,
				styleNode,
				$elm$core$List$reverse(childNodes)));
	});
var $rtfeldman$elm_css$VirtualDom$Styled$toUnstyled = function (vdom) {
	switch (vdom.$) {
		case 'Unstyled':
			var plainNode = vdom.a;
			return plainNode;
		case 'Node':
			var elemType = vdom.a;
			var properties = vdom.b;
			var children = vdom.c;
			return A4($rtfeldman$elm_css$VirtualDom$Styled$unstyle, $elm$core$Maybe$Nothing, elemType, properties, children);
		case 'NodeNS':
			var ns = vdom.a;
			var elemType = vdom.b;
			var properties = vdom.c;
			var children = vdom.d;
			return A5($rtfeldman$elm_css$VirtualDom$Styled$unstyleNS, $elm$core$Maybe$Nothing, ns, elemType, properties, children);
		case 'KeyedNode':
			var elemType = vdom.a;
			var properties = vdom.b;
			var children = vdom.c;
			return A4($rtfeldman$elm_css$VirtualDom$Styled$unstyleKeyed, $elm$core$Maybe$Nothing, elemType, properties, children);
		default:
			var ns = vdom.a;
			var elemType = vdom.b;
			var properties = vdom.c;
			var children = vdom.d;
			return A5($rtfeldman$elm_css$VirtualDom$Styled$unstyleKeyedNS, $elm$core$Maybe$Nothing, ns, elemType, properties, children);
	}
};
var $rtfeldman$elm_css$Html$Styled$toUnstyled = $rtfeldman$elm_css$VirtualDom$Styled$toUnstyled;
var $elm$core$String$trim = _String_trim;
var $rtfeldman$elm_css$Html$Styled$Attributes$type_ = $rtfeldman$elm_css$Html$Styled$Attributes$stringProperty('type');
var $author$project$Main$view = function (model) {
	return {
		body: A2(
			$elm$core$List$map,
			$rtfeldman$elm_css$Html$Styled$toUnstyled,
			_Utils_ap(
				_List_fromArray(
					[
						$BrianHicks$elm_css_reset$Css$Reset$meyerV2,
						$BrianHicks$elm_css_reset$Css$Reset$borderBoxV201408,
						A3(
						$rtfeldman$elm_css$Html$Styled$node,
						'style',
						_List_Nil,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$text('@keyframes spin { from { transform: rotate(0deg); } to { transform: rotate(360deg); } } .fade-overlay { animation: fadeIn 0.5s; } @keyframes fadeIn { from { opacity: 0; } to { opacity: 1; } }')
							])),
						A2(
						$rtfeldman$elm_css$Html$Styled$div,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$Attributes$css(
								_List_fromArray(
									[
										$rtfeldman$elm_css$Css$width(
										$rtfeldman$elm_css$Css$pct(100))
									]))
							]),
						_List_fromArray(
							[
								A2(
								$rtfeldman$elm_css$Html$Styled$main_,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$Attributes$css(
										_List_fromArray(
											[
												$rtfeldman$elm_css$Css$maxWidth(
												$rtfeldman$elm_css$Css$px(1024)),
												A2($rtfeldman$elm_css$Css$margin2, $rtfeldman$elm_css$Css$zero, $rtfeldman$elm_css$Css$auto)
											]))
									]),
								_List_fromArray(
									[
										$author$project$Main$currentMatch(model),
										$author$project$Main$filterBar(model),
										$author$project$Main$rankings(model)
									]))
							]))
					]),
				_Utils_ap(
					model.isSyncing ? _List_fromArray(
						[
							A2(
							$rtfeldman$elm_css$Html$Styled$div,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$css(
									_List_fromArray(
										[
											$rtfeldman$elm_css$Css$position($rtfeldman$elm_css$Css$fixed),
											$rtfeldman$elm_css$Css$top($rtfeldman$elm_css$Css$zero),
											$rtfeldman$elm_css$Css$left($rtfeldman$elm_css$Css$zero),
											$rtfeldman$elm_css$Css$width(
											$rtfeldman$elm_css$Css$pct(100)),
											$rtfeldman$elm_css$Css$height(
											$rtfeldman$elm_css$Css$pct(100)),
											$rtfeldman$elm_css$Css$backgroundColor(
											A4($rtfeldman$elm_css$Css$rgba, 0, 0, 0, 0.6)),
											$rtfeldman$elm_css$Css$displayFlex,
											$rtfeldman$elm_css$Css$alignItems($rtfeldman$elm_css$Css$center),
											$rtfeldman$elm_css$Css$justifyContent($rtfeldman$elm_css$Css$center),
											$rtfeldman$elm_css$Css$zIndex(
											$rtfeldman$elm_css$Css$int(2000))
										])),
									$rtfeldman$elm_css$Html$Styled$Attributes$class('fade-overlay')
								]),
							_List_fromArray(
								[
									A2(
									$rtfeldman$elm_css$Html$Styled$div,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$css(
											_List_fromArray(
												[
													$rtfeldman$elm_css$Css$backgroundColor(
													$rtfeldman$elm_css$Css$hex('FFFFFF')),
													$rtfeldman$elm_css$Css$borderRadius(
													$rtfeldman$elm_css$Css$px(12)),
													$rtfeldman$elm_css$Css$padding(
													$rtfeldman$elm_css$Css$px(32)),
													A4(
													$rtfeldman$elm_css$Css$boxShadow4,
													$rtfeldman$elm_css$Css$px(0),
													$rtfeldman$elm_css$Css$px(8),
													$rtfeldman$elm_css$Css$px(32),
													A4($rtfeldman$elm_css$Css$rgba, 0, 0, 0, 0.3)),
													$rtfeldman$elm_css$Css$textAlign($rtfeldman$elm_css$Css$center),
													$author$project$Main$modernSansSerif
												]))
										]),
									_List_fromArray(
										[
											A2(
											$rtfeldman$elm_css$Html$Styled$div,
											_List_fromArray(
												[
													$rtfeldman$elm_css$Html$Styled$Attributes$css(
													_List_fromArray(
														[
															$rtfeldman$elm_css$Css$width(
															$rtfeldman$elm_css$Css$px(40)),
															$rtfeldman$elm_css$Css$height(
															$rtfeldman$elm_css$Css$px(40)),
															A3(
															$rtfeldman$elm_css$Css$border3,
															$rtfeldman$elm_css$Css$px(4),
															$rtfeldman$elm_css$Css$solid,
															$rtfeldman$elm_css$Css$hex('E5E7EB')),
															$rtfeldman$elm_css$Css$borderTopColor(
															$rtfeldman$elm_css$Css$hex('3B82F6')),
															$rtfeldman$elm_css$Css$borderRadius(
															$rtfeldman$elm_css$Css$pct(50)),
															A2($rtfeldman$elm_css$Css$property, 'animation', 'spin 1s linear infinite'),
															A2(
															$rtfeldman$elm_css$Css$margin2,
															$rtfeldman$elm_css$Css$px(0),
															$rtfeldman$elm_css$Css$auto),
															$rtfeldman$elm_css$Css$marginBottom(
															$rtfeldman$elm_css$Css$px(16))
														]))
												]),
											_List_Nil),
											A2(
											$rtfeldman$elm_css$Html$Styled$p,
											_List_fromArray(
												[
													$rtfeldman$elm_css$Html$Styled$Attributes$css(
													_List_fromArray(
														[
															$rtfeldman$elm_css$Css$fontSize(
															$rtfeldman$elm_css$Css$px(16)),
															$rtfeldman$elm_css$Css$fontWeight(
															$rtfeldman$elm_css$Css$int(500)),
															$rtfeldman$elm_css$Css$color(
															$rtfeldman$elm_css$Css$hex('1F2937')),
															$rtfeldman$elm_css$Css$margin($rtfeldman$elm_css$Css$zero)
														]))
												]),
											_List_fromArray(
												[
													$rtfeldman$elm_css$Html$Styled$text('Syncing with database...')
												])),
											A2(
											$rtfeldman$elm_css$Html$Styled$p,
											_List_fromArray(
												[
													$rtfeldman$elm_css$Html$Styled$Attributes$css(
													_List_fromArray(
														[
															$rtfeldman$elm_css$Css$fontSize(
															$rtfeldman$elm_css$Css$px(14)),
															$rtfeldman$elm_css$Css$color(
															$rtfeldman$elm_css$Css$hex('6B7280')),
															$rtfeldman$elm_css$Css$margin($rtfeldman$elm_css$Css$zero),
															$rtfeldman$elm_css$Css$marginTop(
															$rtfeldman$elm_css$Css$px(8))
														]))
												]),
											_List_fromArray(
												[
													$rtfeldman$elm_css$Html$Styled$text('Your next match will start shortly')
												]))
										]))
								]))
						]) : _List_Nil,
					_Utils_ap(
						function () {
							var _v0 = model.playerDeletionConfirmation;
							if (_v0.$ === 'Just') {
								var _v1 = _v0.a;
								var player = _v1.a;
								var step = _v1.b;
								return _List_fromArray(
									[
										A2(
										$rtfeldman$elm_css$Html$Styled$div,
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$Attributes$css(
												_List_fromArray(
													[
														$rtfeldman$elm_css$Css$position($rtfeldman$elm_css$Css$fixed),
														$rtfeldman$elm_css$Css$top($rtfeldman$elm_css$Css$zero),
														$rtfeldman$elm_css$Css$left($rtfeldman$elm_css$Css$zero),
														$rtfeldman$elm_css$Css$width(
														$rtfeldman$elm_css$Css$pct(100)),
														$rtfeldman$elm_css$Css$height(
														$rtfeldman$elm_css$Css$pct(100)),
														$rtfeldman$elm_css$Css$backgroundColor(
														A4($rtfeldman$elm_css$Css$rgba, 0, 0, 0, 0.5)),
														$rtfeldman$elm_css$Css$displayFlex,
														$rtfeldman$elm_css$Css$alignItems($rtfeldman$elm_css$Css$center),
														$rtfeldman$elm_css$Css$justifyContent($rtfeldman$elm_css$Css$center),
														$rtfeldman$elm_css$Css$zIndex(
														$rtfeldman$elm_css$Css$int(1500))
													]))
											]),
										_List_fromArray(
											[
												A2(
												$rtfeldman$elm_css$Html$Styled$div,
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$Attributes$css(
														_List_fromArray(
															[
																$rtfeldman$elm_css$Css$backgroundColor(
																$rtfeldman$elm_css$Css$hex('FFFFFF')),
																$rtfeldman$elm_css$Css$borderRadius(
																$rtfeldman$elm_css$Css$px(12)),
																$rtfeldman$elm_css$Css$padding(
																$rtfeldman$elm_css$Css$px(24)),
																A4(
																$rtfeldman$elm_css$Css$boxShadow4,
																$rtfeldman$elm_css$Css$px(0),
																$rtfeldman$elm_css$Css$px(8),
																$rtfeldman$elm_css$Css$px(32),
																A4($rtfeldman$elm_css$Css$rgba, 0, 0, 0, 0.3)),
																$rtfeldman$elm_css$Css$maxWidth(
																$rtfeldman$elm_css$Css$px(400)),
																$rtfeldman$elm_css$Css$textAlign($rtfeldman$elm_css$Css$center),
																$author$project$Main$modernSansSerif
															]))
													]),
												_List_fromArray(
													[
														A2(
														$rtfeldman$elm_css$Html$Styled$h3,
														_List_fromArray(
															[
																$rtfeldman$elm_css$Html$Styled$Attributes$css(
																_List_fromArray(
																	[
																		A2(
																		$rtfeldman$elm_css$Css$margin2,
																		$rtfeldman$elm_css$Css$px(0),
																		$rtfeldman$elm_css$Css$px(0)),
																		$rtfeldman$elm_css$Css$marginBottom(
																		$rtfeldman$elm_css$Css$px(16)),
																		$rtfeldman$elm_css$Css$fontSize(
																		$rtfeldman$elm_css$Css$px(18)),
																		$rtfeldman$elm_css$Css$fontWeight(
																		$rtfeldman$elm_css$Css$int(600)),
																		$rtfeldman$elm_css$Css$color(
																		$rtfeldman$elm_css$Css$hex('DC2626'))
																	]))
															]),
														_List_fromArray(
															[
																$rtfeldman$elm_css$Html$Styled$text(
																(step === 1) ? 'Delete Player?' : 'Final Warning!')
															])),
														A2(
														$rtfeldman$elm_css$Html$Styled$p,
														_List_fromArray(
															[
																$rtfeldman$elm_css$Html$Styled$Attributes$css(
																_List_fromArray(
																	[
																		A2(
																		$rtfeldman$elm_css$Css$margin2,
																		$rtfeldman$elm_css$Css$px(0),
																		$rtfeldman$elm_css$Css$px(0)),
																		$rtfeldman$elm_css$Css$marginBottom(
																		$rtfeldman$elm_css$Css$px(24)),
																		$rtfeldman$elm_css$Css$fontSize(
																		$rtfeldman$elm_css$Css$px(16)),
																		$rtfeldman$elm_css$Css$color(
																		$rtfeldman$elm_css$Css$hex('374151'))
																	]))
															]),
														_List_fromArray(
															[
																$rtfeldman$elm_css$Html$Styled$text(
																(step === 1) ? ('Are you sure you want to delete ' + ($author$project$Player$name(player) + '?')) : 'Is that your final answer? This cannot be undone!')
															])),
														A2(
														$rtfeldman$elm_css$Html$Styled$div,
														_List_fromArray(
															[
																$rtfeldman$elm_css$Html$Styled$Attributes$css(
																_List_fromArray(
																	[
																		$rtfeldman$elm_css$Css$displayFlex,
																		$rtfeldman$elm_css$Css$justifyContent($rtfeldman$elm_css$Css$center)
																	]))
															]),
														_List_fromArray(
															[
																A2(
																$rtfeldman$elm_css$Html$Styled$button,
																_List_fromArray(
																	[
																		$rtfeldman$elm_css$Html$Styled$Attributes$css(
																		_List_fromArray(
																			[
																				$rtfeldman$elm_css$Css$backgroundColor(
																				$rtfeldman$elm_css$Css$hex('6B7280')),
																				$rtfeldman$elm_css$Css$color(
																				$rtfeldman$elm_css$Css$hex('FFFFFF')),
																				$rtfeldman$elm_css$Css$border($rtfeldman$elm_css$Css$zero),
																				$rtfeldman$elm_css$Css$borderRadius(
																				$rtfeldman$elm_css$Css$px(6)),
																				A2(
																				$rtfeldman$elm_css$Css$padding2,
																				$rtfeldman$elm_css$Css$px(8),
																				$rtfeldman$elm_css$Css$px(16)),
																				$rtfeldman$elm_css$Css$cursor($rtfeldman$elm_css$Css$pointer),
																				$rtfeldman$elm_css$Css$fontSize(
																				$rtfeldman$elm_css$Css$px(14)),
																				$rtfeldman$elm_css$Css$fontWeight(
																				$rtfeldman$elm_css$Css$int(500)),
																				$rtfeldman$elm_css$Css$marginRight(
																				$rtfeldman$elm_css$Css$px(12)),
																				$rtfeldman$elm_css$Css$hover(
																				_List_fromArray(
																					[
																						$rtfeldman$elm_css$Css$backgroundColor(
																						$rtfeldman$elm_css$Css$hex('4B5563'))
																					]))
																			])),
																		$rtfeldman$elm_css$Html$Styled$Events$onClick($author$project$Main$CancelPlayerDeletion)
																	]),
																_List_fromArray(
																	[
																		$rtfeldman$elm_css$Html$Styled$text('Cancel')
																	])),
																A2(
																$rtfeldman$elm_css$Html$Styled$button,
																_List_fromArray(
																	[
																		$rtfeldman$elm_css$Html$Styled$Attributes$css(
																		_List_fromArray(
																			[
																				$rtfeldman$elm_css$Css$backgroundColor(
																				$rtfeldman$elm_css$Css$hex('DC2626')),
																				$rtfeldman$elm_css$Css$color(
																				$rtfeldman$elm_css$Css$hex('FFFFFF')),
																				$rtfeldman$elm_css$Css$border($rtfeldman$elm_css$Css$zero),
																				$rtfeldman$elm_css$Css$borderRadius(
																				$rtfeldman$elm_css$Css$px(6)),
																				A2(
																				$rtfeldman$elm_css$Css$padding2,
																				$rtfeldman$elm_css$Css$px(8),
																				$rtfeldman$elm_css$Css$px(16)),
																				$rtfeldman$elm_css$Css$cursor($rtfeldman$elm_css$Css$pointer),
																				$rtfeldman$elm_css$Css$fontSize(
																				$rtfeldman$elm_css$Css$px(14)),
																				$rtfeldman$elm_css$Css$fontWeight(
																				$rtfeldman$elm_css$Css$int(500)),
																				$rtfeldman$elm_css$Css$hover(
																				_List_fromArray(
																					[
																						$rtfeldman$elm_css$Css$backgroundColor(
																						$rtfeldman$elm_css$Css$hex('B91C1C'))
																					]))
																			])),
																		$rtfeldman$elm_css$Html$Styled$Events$onClick(
																		A2($author$project$Main$ConfirmPlayerDeletion, player, step + 1))
																	]),
																_List_fromArray(
																	[
																		$rtfeldman$elm_css$Html$Styled$text(
																		(step === 1) ? 'Yes, Delete' : 'Final Answer: DELETE')
																	]))
															]))
													]))
											]))
									]);
							} else {
								return _List_Nil;
							}
						}(),
						_Utils_ap(
							function () {
								var _v2 = model.timeToggleConfirmation;
								if (_v2.$ === 'Just') {
									var _v3 = _v2.a;
									var player = _v3.a;
									var timeType = _v3.b;
									var currentState = (timeType === 'AM') ? $author$project$Player$playsAM(player) : $author$project$Player$playsPM(player);
									var newState = !currentState;
									return _List_fromArray(
										[
											A2(
											$rtfeldman$elm_css$Html$Styled$div,
											_List_fromArray(
												[
													$rtfeldman$elm_css$Html$Styled$Attributes$css(
													_List_fromArray(
														[
															$rtfeldman$elm_css$Css$position($rtfeldman$elm_css$Css$fixed),
															$rtfeldman$elm_css$Css$top($rtfeldman$elm_css$Css$zero),
															$rtfeldman$elm_css$Css$left($rtfeldman$elm_css$Css$zero),
															$rtfeldman$elm_css$Css$width(
															$rtfeldman$elm_css$Css$pct(100)),
															$rtfeldman$elm_css$Css$height(
															$rtfeldman$elm_css$Css$pct(100)),
															$rtfeldman$elm_css$Css$backgroundColor(
															A4($rtfeldman$elm_css$Css$rgba, 0, 0, 0, 0.5)),
															$rtfeldman$elm_css$Css$displayFlex,
															$rtfeldman$elm_css$Css$alignItems($rtfeldman$elm_css$Css$center),
															$rtfeldman$elm_css$Css$justifyContent($rtfeldman$elm_css$Css$center),
															$rtfeldman$elm_css$Css$zIndex(
															$rtfeldman$elm_css$Css$int(1500))
														]))
												]),
											_List_fromArray(
												[
													A2(
													$rtfeldman$elm_css$Html$Styled$div,
													_List_fromArray(
														[
															$rtfeldman$elm_css$Html$Styled$Attributes$css(
															_List_fromArray(
																[
																	$rtfeldman$elm_css$Css$backgroundColor(
																	$rtfeldman$elm_css$Css$hex('FFFFFF')),
																	$rtfeldman$elm_css$Css$borderRadius(
																	$rtfeldman$elm_css$Css$px(12)),
																	$rtfeldman$elm_css$Css$padding(
																	$rtfeldman$elm_css$Css$px(24)),
																	A4(
																	$rtfeldman$elm_css$Css$boxShadow4,
																	$rtfeldman$elm_css$Css$px(0),
																	$rtfeldman$elm_css$Css$px(8),
																	$rtfeldman$elm_css$Css$px(32),
																	A4($rtfeldman$elm_css$Css$rgba, 0, 0, 0, 0.3)),
																	$rtfeldman$elm_css$Css$maxWidth(
																	$rtfeldman$elm_css$Css$px(400)),
																	$rtfeldman$elm_css$Css$textAlign($rtfeldman$elm_css$Css$center),
																	$author$project$Main$modernSansSerif
																]))
														]),
													_List_fromArray(
														[
															A2(
															$rtfeldman$elm_css$Html$Styled$h3,
															_List_fromArray(
																[
																	$rtfeldman$elm_css$Html$Styled$Attributes$css(
																	_List_fromArray(
																		[
																			A2(
																			$rtfeldman$elm_css$Css$margin2,
																			$rtfeldman$elm_css$Css$px(0),
																			$rtfeldman$elm_css$Css$px(0)),
																			$rtfeldman$elm_css$Css$marginBottom(
																			$rtfeldman$elm_css$Css$px(16)),
																			$rtfeldman$elm_css$Css$fontSize(
																			$rtfeldman$elm_css$Css$px(18)),
																			$rtfeldman$elm_css$Css$fontWeight(
																			$rtfeldman$elm_css$Css$int(600)),
																			$rtfeldman$elm_css$Css$color(
																			$rtfeldman$elm_css$Css$hex('DC2626'))
																		]))
																]),
															_List_fromArray(
																[
																	$rtfeldman$elm_css$Html$Styled$text('Confirm ' + (timeType + ' Toggle'))
																])),
															A2(
															$rtfeldman$elm_css$Html$Styled$p,
															_List_fromArray(
																[
																	$rtfeldman$elm_css$Html$Styled$Attributes$css(
																	_List_fromArray(
																		[
																			A2(
																			$rtfeldman$elm_css$Css$margin2,
																			$rtfeldman$elm_css$Css$px(0),
																			$rtfeldman$elm_css$Css$px(0)),
																			$rtfeldman$elm_css$Css$marginBottom(
																			$rtfeldman$elm_css$Css$px(24)),
																			$rtfeldman$elm_css$Css$fontSize(
																			$rtfeldman$elm_css$Css$px(16)),
																			$rtfeldman$elm_css$Css$color(
																			$rtfeldman$elm_css$Css$hex('374151'))
																		]))
																]),
															_List_fromArray(
																[
																	$rtfeldman$elm_css$Html$Styled$text(
																	'Are you sure you want to ' + ((newState ? 'enable' : 'disable') + (' ' + (timeType + (' availability for ' + ($author$project$Player$name(player) + '?'))))))
																])),
															A2(
															$rtfeldman$elm_css$Html$Styled$div,
															_List_fromArray(
																[
																	$rtfeldman$elm_css$Html$Styled$Attributes$css(
																	_List_fromArray(
																		[
																			$rtfeldman$elm_css$Css$displayFlex,
																			$rtfeldman$elm_css$Css$justifyContent($rtfeldman$elm_css$Css$center)
																		]))
																]),
															_List_fromArray(
																[
																	A2(
																	$rtfeldman$elm_css$Html$Styled$button,
																	_List_fromArray(
																		[
																			$rtfeldman$elm_css$Html$Styled$Attributes$css(
																			_List_fromArray(
																				[
																					$rtfeldman$elm_css$Css$backgroundColor(
																					$rtfeldman$elm_css$Css$hex('6B7280')),
																					$rtfeldman$elm_css$Css$color(
																					$rtfeldman$elm_css$Css$hex('FFFFFF')),
																					$rtfeldman$elm_css$Css$border($rtfeldman$elm_css$Css$zero),
																					$rtfeldman$elm_css$Css$borderRadius(
																					$rtfeldman$elm_css$Css$px(6)),
																					A2(
																					$rtfeldman$elm_css$Css$padding2,
																					$rtfeldman$elm_css$Css$px(8),
																					$rtfeldman$elm_css$Css$px(16)),
																					$rtfeldman$elm_css$Css$cursor($rtfeldman$elm_css$Css$pointer),
																					$rtfeldman$elm_css$Css$fontSize(
																					$rtfeldman$elm_css$Css$px(14)),
																					$rtfeldman$elm_css$Css$fontWeight(
																					$rtfeldman$elm_css$Css$int(500)),
																					$rtfeldman$elm_css$Css$marginRight(
																					$rtfeldman$elm_css$Css$px(12)),
																					$rtfeldman$elm_css$Css$hover(
																					_List_fromArray(
																						[
																							$rtfeldman$elm_css$Css$backgroundColor(
																							$rtfeldman$elm_css$Css$hex('4B5563'))
																						]))
																				])),
																			$rtfeldman$elm_css$Html$Styled$Events$onClick($author$project$Main$CancelTogglePlayerTime)
																		]),
																	_List_fromArray(
																		[
																			$rtfeldman$elm_css$Html$Styled$text('Cancel')
																		])),
																	A2(
																	$rtfeldman$elm_css$Html$Styled$button,
																	_List_fromArray(
																		[
																			$rtfeldman$elm_css$Html$Styled$Attributes$css(
																			_List_fromArray(
																				[
																					$rtfeldman$elm_css$Css$backgroundColor(
																					$rtfeldman$elm_css$Css$hex('F59E0B')),
																					$rtfeldman$elm_css$Css$color(
																					$rtfeldman$elm_css$Css$hex('FFFFFF')),
																					$rtfeldman$elm_css$Css$border($rtfeldman$elm_css$Css$zero),
																					$rtfeldman$elm_css$Css$borderRadius(
																					$rtfeldman$elm_css$Css$px(6)),
																					A2(
																					$rtfeldman$elm_css$Css$padding2,
																					$rtfeldman$elm_css$Css$px(8),
																					$rtfeldman$elm_css$Css$px(16)),
																					$rtfeldman$elm_css$Css$cursor($rtfeldman$elm_css$Css$pointer),
																					$rtfeldman$elm_css$Css$fontSize(
																					$rtfeldman$elm_css$Css$px(14)),
																					$rtfeldman$elm_css$Css$fontWeight(
																					$rtfeldman$elm_css$Css$int(500)),
																					$rtfeldman$elm_css$Css$hover(
																					_List_fromArray(
																						[
																							$rtfeldman$elm_css$Css$backgroundColor(
																							$rtfeldman$elm_css$Css$hex('D97706'))
																						]))
																				])),
																			$rtfeldman$elm_css$Html$Styled$Events$onClick(
																			A3($author$project$Main$ConfirmTogglePlayerTime, player, timeType, newState))
																		]),
																	_List_fromArray(
																		[
																			$rtfeldman$elm_css$Html$Styled$text(
																			newState ? ('Yes, Enable ' + timeType) : ('Yes, Disable ' + timeType))
																		]))
																]))
														]))
												]))
										]);
								} else {
									return _List_Nil;
								}
							}(),
							_Utils_ap(
								model.showAddPlayerPopup ? _List_fromArray(
									[
										A2(
										$rtfeldman$elm_css$Html$Styled$div,
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$Attributes$css(
												_List_fromArray(
													[
														$rtfeldman$elm_css$Css$position($rtfeldman$elm_css$Css$fixed),
														$rtfeldman$elm_css$Css$top($rtfeldman$elm_css$Css$zero),
														$rtfeldman$elm_css$Css$left($rtfeldman$elm_css$Css$zero),
														$rtfeldman$elm_css$Css$width(
														$rtfeldman$elm_css$Css$pct(100)),
														$rtfeldman$elm_css$Css$height(
														$rtfeldman$elm_css$Css$pct(100)),
														$rtfeldman$elm_css$Css$backgroundColor(
														A4($rtfeldman$elm_css$Css$rgba, 0, 0, 0, 0.5)),
														$rtfeldman$elm_css$Css$displayFlex,
														$rtfeldman$elm_css$Css$alignItems($rtfeldman$elm_css$Css$center),
														$rtfeldman$elm_css$Css$justifyContent($rtfeldman$elm_css$Css$center),
														$rtfeldman$elm_css$Css$zIndex(
														$rtfeldman$elm_css$Css$int(1500))
													]))
											]),
										_List_fromArray(
											[
												A2(
												$rtfeldman$elm_css$Html$Styled$div,
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$Attributes$css(
														_List_fromArray(
															[
																$rtfeldman$elm_css$Css$backgroundColor(
																$rtfeldman$elm_css$Css$hex('FFFFFF')),
																$rtfeldman$elm_css$Css$borderRadius(
																$rtfeldman$elm_css$Css$px(12)),
																$rtfeldman$elm_css$Css$padding(
																$rtfeldman$elm_css$Css$px(32)),
																A4(
																$rtfeldman$elm_css$Css$boxShadow4,
																$rtfeldman$elm_css$Css$px(0),
																$rtfeldman$elm_css$Css$px(20),
																$rtfeldman$elm_css$Css$px(25),
																A4($rtfeldman$elm_css$Css$rgba, 0, 0, 0, 0.15)),
																$rtfeldman$elm_css$Css$maxWidth(
																$rtfeldman$elm_css$Css$px(500)),
																$rtfeldman$elm_css$Css$width(
																$rtfeldman$elm_css$Css$pct(90)),
																$author$project$Main$modernSansSerif
															]))
													]),
												_List_fromArray(
													[
														A2(
														$rtfeldman$elm_css$Html$Styled$h2,
														_List_fromArray(
															[
																$rtfeldman$elm_css$Html$Styled$Attributes$css(
																_List_fromArray(
																	[
																		$rtfeldman$elm_css$Css$fontSize(
																		$rtfeldman$elm_css$Css$px(24)),
																		$rtfeldman$elm_css$Css$fontWeight(
																		$rtfeldman$elm_css$Css$int(600)),
																		$rtfeldman$elm_css$Css$color(
																		$rtfeldman$elm_css$Css$hex('1F2937')),
																		$rtfeldman$elm_css$Css$marginBottom(
																		$rtfeldman$elm_css$Css$px(24)),
																		$rtfeldman$elm_css$Css$textAlign($rtfeldman$elm_css$Css$center)
																	]))
															]),
														_List_fromArray(
															[
																$rtfeldman$elm_css$Html$Styled$text('Add New Player')
															])),
														A2(
														$rtfeldman$elm_css$Html$Styled$div,
														_List_fromArray(
															[
																$rtfeldman$elm_css$Html$Styled$Attributes$css(
																_List_fromArray(
																	[
																		$rtfeldman$elm_css$Css$marginBottom(
																		$rtfeldman$elm_css$Css$px(20))
																	]))
															]),
														_List_fromArray(
															[
																A2(
																$rtfeldman$elm_css$Html$Styled$label,
																_List_fromArray(
																	[
																		$rtfeldman$elm_css$Html$Styled$Attributes$css(
																		_List_fromArray(
																			[
																				$rtfeldman$elm_css$Css$display($rtfeldman$elm_css$Css$block),
																				$rtfeldman$elm_css$Css$fontSize(
																				$rtfeldman$elm_css$Css$px(14)),
																				$rtfeldman$elm_css$Css$fontWeight(
																				$rtfeldman$elm_css$Css$int(600)),
																				$rtfeldman$elm_css$Css$color(
																				$rtfeldman$elm_css$Css$hex('374151')),
																				$rtfeldman$elm_css$Css$marginBottom(
																				$rtfeldman$elm_css$Css$px(6))
																			]))
																	]),
																_List_fromArray(
																	[
																		$rtfeldman$elm_css$Html$Styled$text('Player Name')
																	])),
																A2(
																$rtfeldman$elm_css$Html$Styled$input,
																_List_fromArray(
																	[
																		$rtfeldman$elm_css$Html$Styled$Attributes$type_('text'),
																		$rtfeldman$elm_css$Html$Styled$Attributes$placeholder('Enter player name'),
																		$rtfeldman$elm_css$Html$Styled$Attributes$value(model.addPlayerName),
																		$rtfeldman$elm_css$Html$Styled$Events$onInput($author$project$Main$KeeperUpdatedAddPlayerName),
																		$rtfeldman$elm_css$Html$Styled$Attributes$css(
																		_List_fromArray(
																			[
																				$rtfeldman$elm_css$Css$width(
																				$rtfeldman$elm_css$Css$pct(100)),
																				A2(
																				$rtfeldman$elm_css$Css$padding2,
																				$rtfeldman$elm_css$Css$px(12),
																				$rtfeldman$elm_css$Css$px(16)),
																				A3(
																				$rtfeldman$elm_css$Css$border3,
																				$rtfeldman$elm_css$Css$px(2),
																				$rtfeldman$elm_css$Css$solid,
																				$rtfeldman$elm_css$Css$hex('E5E7EB')),
																				$rtfeldman$elm_css$Css$borderRadius(
																				$rtfeldman$elm_css$Css$px(8)),
																				$rtfeldman$elm_css$Css$fontSize(
																				$rtfeldman$elm_css$Css$px(16)),
																				$rtfeldman$elm_css$Css$focus(
																				_List_fromArray(
																					[
																						$rtfeldman$elm_css$Css$borderColor(
																						$rtfeldman$elm_css$Css$hex('3B82F6')),
																						$rtfeldman$elm_css$Css$outline($rtfeldman$elm_css$Css$none)
																					])),
																				$rtfeldman$elm_css$Css$boxSizing($rtfeldman$elm_css$Css$borderBox)
																			]))
																	]),
																_List_Nil)
															])),
														A2(
														$rtfeldman$elm_css$Html$Styled$div,
														_List_fromArray(
															[
																$rtfeldman$elm_css$Html$Styled$Attributes$css(
																_List_fromArray(
																	[
																		$rtfeldman$elm_css$Css$marginBottom(
																		$rtfeldman$elm_css$Css$px(20))
																	]))
															]),
														_List_fromArray(
															[
																A2(
																$rtfeldman$elm_css$Html$Styled$label,
																_List_fromArray(
																	[
																		$rtfeldman$elm_css$Html$Styled$Attributes$css(
																		_List_fromArray(
																			[
																				$rtfeldman$elm_css$Css$display($rtfeldman$elm_css$Css$block),
																				$rtfeldman$elm_css$Css$fontSize(
																				$rtfeldman$elm_css$Css$px(14)),
																				$rtfeldman$elm_css$Css$fontWeight(
																				$rtfeldman$elm_css$Css$int(600)),
																				$rtfeldman$elm_css$Css$color(
																				$rtfeldman$elm_css$Css$hex('374151')),
																				$rtfeldman$elm_css$Css$marginBottom(
																				$rtfeldman$elm_css$Css$px(6))
																			]))
																	]),
																_List_fromArray(
																	[
																		$rtfeldman$elm_css$Html$Styled$text('Estimated Rating')
																	])),
																A2(
																$rtfeldman$elm_css$Html$Styled$input,
																_List_fromArray(
																	[
																		$rtfeldman$elm_css$Html$Styled$Attributes$type_('number'),
																		$rtfeldman$elm_css$Html$Styled$Attributes$placeholder('500'),
																		$rtfeldman$elm_css$Html$Styled$Attributes$value(model.addPlayerRating),
																		$rtfeldman$elm_css$Html$Styled$Events$onInput($author$project$Main$KeeperUpdatedAddPlayerRating),
																		$rtfeldman$elm_css$Html$Styled$Attributes$css(
																		_List_fromArray(
																			[
																				$rtfeldman$elm_css$Css$width(
																				$rtfeldman$elm_css$Css$pct(100)),
																				A2(
																				$rtfeldman$elm_css$Css$padding2,
																				$rtfeldman$elm_css$Css$px(12),
																				$rtfeldman$elm_css$Css$px(16)),
																				A3(
																				$rtfeldman$elm_css$Css$border3,
																				$rtfeldman$elm_css$Css$px(2),
																				$rtfeldman$elm_css$Css$solid,
																				$rtfeldman$elm_css$Css$hex('E5E7EB')),
																				$rtfeldman$elm_css$Css$borderRadius(
																				$rtfeldman$elm_css$Css$px(8)),
																				$rtfeldman$elm_css$Css$fontSize(
																				$rtfeldman$elm_css$Css$px(16)),
																				$rtfeldman$elm_css$Css$focus(
																				_List_fromArray(
																					[
																						$rtfeldman$elm_css$Css$borderColor(
																						$rtfeldman$elm_css$Css$hex('3B82F6')),
																						$rtfeldman$elm_css$Css$outline($rtfeldman$elm_css$Css$none)
																					])),
																				$rtfeldman$elm_css$Css$boxSizing($rtfeldman$elm_css$Css$borderBox)
																			]))
																	]),
																_List_Nil)
															])),
														A2(
														$rtfeldman$elm_css$Html$Styled$div,
														_List_fromArray(
															[
																$rtfeldman$elm_css$Html$Styled$Attributes$css(
																_List_fromArray(
																	[
																		$rtfeldman$elm_css$Css$marginBottom(
																		$rtfeldman$elm_css$Css$px(24))
																	]))
															]),
														_List_fromArray(
															[
																A2(
																$rtfeldman$elm_css$Html$Styled$label,
																_List_fromArray(
																	[
																		$rtfeldman$elm_css$Html$Styled$Attributes$css(
																		_List_fromArray(
																			[
																				$rtfeldman$elm_css$Css$display($rtfeldman$elm_css$Css$block),
																				$rtfeldman$elm_css$Css$fontSize(
																				$rtfeldman$elm_css$Css$px(14)),
																				$rtfeldman$elm_css$Css$fontWeight(
																				$rtfeldman$elm_css$Css$int(600)),
																				$rtfeldman$elm_css$Css$color(
																				$rtfeldman$elm_css$Css$hex('374151')),
																				$rtfeldman$elm_css$Css$marginBottom(
																				$rtfeldman$elm_css$Css$px(12))
																			]))
																	]),
																_List_fromArray(
																	[
																		$rtfeldman$elm_css$Html$Styled$text('Available Times')
																	])),
																A2(
																$rtfeldman$elm_css$Html$Styled$div,
																_List_fromArray(
																	[
																		$rtfeldman$elm_css$Html$Styled$Attributes$css(
																		_List_fromArray(
																			[
																				$rtfeldman$elm_css$Css$displayFlex,
																				$rtfeldman$elm_css$Css$justifyContent($rtfeldman$elm_css$Css$spaceAround)
																			]))
																	]),
																_List_fromArray(
																	[
																		A2(
																		$rtfeldman$elm_css$Html$Styled$label,
																		_List_fromArray(
																			[
																				$rtfeldman$elm_css$Html$Styled$Attributes$css(
																				_List_fromArray(
																					[
																						$rtfeldman$elm_css$Css$displayFlex,
																						$rtfeldman$elm_css$Css$alignItems($rtfeldman$elm_css$Css$center),
																						$rtfeldman$elm_css$Css$cursor($rtfeldman$elm_css$Css$pointer),
																						$rtfeldman$elm_css$Css$fontSize(
																						$rtfeldman$elm_css$Css$px(16))
																					]))
																			]),
																		_List_fromArray(
																			[
																				A2(
																				$rtfeldman$elm_css$Html$Styled$input,
																				_List_fromArray(
																					[
																						$rtfeldman$elm_css$Html$Styled$Attributes$type_('checkbox'),
																						$rtfeldman$elm_css$Html$Styled$Attributes$checked(model.addPlayerAM),
																						$rtfeldman$elm_css$Html$Styled$Events$onClick($author$project$Main$KeeperToggledAddPlayerAM),
																						$rtfeldman$elm_css$Html$Styled$Attributes$css(
																						_List_fromArray(
																							[
																								$rtfeldman$elm_css$Css$width(
																								$rtfeldman$elm_css$Css$px(20)),
																								$rtfeldman$elm_css$Css$height(
																								$rtfeldman$elm_css$Css$px(20)),
																								$rtfeldman$elm_css$Css$marginRight(
																								$rtfeldman$elm_css$Css$px(8))
																							]))
																					]),
																				_List_Nil),
																				$rtfeldman$elm_css$Html$Styled$text('AM Games')
																			])),
																		A2(
																		$rtfeldman$elm_css$Html$Styled$label,
																		_List_fromArray(
																			[
																				$rtfeldman$elm_css$Html$Styled$Attributes$css(
																				_List_fromArray(
																					[
																						$rtfeldman$elm_css$Css$displayFlex,
																						$rtfeldman$elm_css$Css$alignItems($rtfeldman$elm_css$Css$center),
																						$rtfeldman$elm_css$Css$cursor($rtfeldman$elm_css$Css$pointer),
																						$rtfeldman$elm_css$Css$fontSize(
																						$rtfeldman$elm_css$Css$px(16))
																					]))
																			]),
																		_List_fromArray(
																			[
																				A2(
																				$rtfeldman$elm_css$Html$Styled$input,
																				_List_fromArray(
																					[
																						$rtfeldman$elm_css$Html$Styled$Attributes$type_('checkbox'),
																						$rtfeldman$elm_css$Html$Styled$Attributes$checked(model.addPlayerPM),
																						$rtfeldman$elm_css$Html$Styled$Events$onClick($author$project$Main$KeeperToggledAddPlayerPM),
																						$rtfeldman$elm_css$Html$Styled$Attributes$css(
																						_List_fromArray(
																							[
																								$rtfeldman$elm_css$Css$width(
																								$rtfeldman$elm_css$Css$px(20)),
																								$rtfeldman$elm_css$Css$height(
																								$rtfeldman$elm_css$Css$px(20)),
																								$rtfeldman$elm_css$Css$marginRight(
																								$rtfeldman$elm_css$Css$px(8))
																							]))
																					]),
																				_List_Nil),
																				$rtfeldman$elm_css$Html$Styled$text('PM Games')
																			]))
																	]))
															])),
														A2(
														$rtfeldman$elm_css$Html$Styled$div,
														_List_fromArray(
															[
																$rtfeldman$elm_css$Html$Styled$Attributes$css(
																_List_fromArray(
																	[
																		$rtfeldman$elm_css$Css$displayFlex,
																		$rtfeldman$elm_css$Css$justifyContent($rtfeldman$elm_css$Css$spaceAround)
																	]))
															]),
														_List_fromArray(
															[
																A2(
																$rtfeldman$elm_css$Html$Styled$button,
																_List_fromArray(
																	[
																		$rtfeldman$elm_css$Html$Styled$Attributes$css(
																		_List_fromArray(
																			[
																				$rtfeldman$elm_css$Css$backgroundColor(
																				$rtfeldman$elm_css$Css$hex('6B7280')),
																				$rtfeldman$elm_css$Css$color(
																				$rtfeldman$elm_css$Css$hex('FFFFFF')),
																				$rtfeldman$elm_css$Css$border($rtfeldman$elm_css$Css$zero),
																				$rtfeldman$elm_css$Css$borderRadius(
																				$rtfeldman$elm_css$Css$px(8)),
																				A2(
																				$rtfeldman$elm_css$Css$padding2,
																				$rtfeldman$elm_css$Css$px(12),
																				$rtfeldman$elm_css$Css$px(24)),
																				$rtfeldman$elm_css$Css$cursor($rtfeldman$elm_css$Css$pointer),
																				$rtfeldman$elm_css$Css$fontSize(
																				$rtfeldman$elm_css$Css$px(16)),
																				$rtfeldman$elm_css$Css$fontWeight(
																				$rtfeldman$elm_css$Css$int(500)),
																				$rtfeldman$elm_css$Css$hover(
																				_List_fromArray(
																					[
																						$rtfeldman$elm_css$Css$backgroundColor(
																						$rtfeldman$elm_css$Css$hex('4B5563'))
																					])),
																				$author$project$Main$modernSansSerif
																			])),
																		$rtfeldman$elm_css$Html$Styled$Events$onClick($author$project$Main$KeeperWantsToHideAddPlayerPopup)
																	]),
																_List_fromArray(
																	[
																		$rtfeldman$elm_css$Html$Styled$text('Cancel')
																	])),
																A2(
																$rtfeldman$elm_css$Html$Styled$button,
																_List_fromArray(
																	[
																		$rtfeldman$elm_css$Html$Styled$Attributes$css(
																		_List_fromArray(
																			[
																				$rtfeldman$elm_css$Css$backgroundColor(
																				$rtfeldman$elm_css$Css$hex('10B981')),
																				$rtfeldman$elm_css$Css$color(
																				$rtfeldman$elm_css$Css$hex('FFFFFF')),
																				$rtfeldman$elm_css$Css$border($rtfeldman$elm_css$Css$zero),
																				$rtfeldman$elm_css$Css$borderRadius(
																				$rtfeldman$elm_css$Css$px(8)),
																				A2(
																				$rtfeldman$elm_css$Css$padding2,
																				$rtfeldman$elm_css$Css$px(12),
																				$rtfeldman$elm_css$Css$px(24)),
																				$rtfeldman$elm_css$Css$cursor($rtfeldman$elm_css$Css$pointer),
																				$rtfeldman$elm_css$Css$fontSize(
																				$rtfeldman$elm_css$Css$px(16)),
																				$rtfeldman$elm_css$Css$fontWeight(
																				$rtfeldman$elm_css$Css$int(500)),
																				$rtfeldman$elm_css$Css$hover(
																				_List_fromArray(
																					[
																						$rtfeldman$elm_css$Css$backgroundColor(
																						$rtfeldman$elm_css$Css$hex('059669'))
																					])),
																				$author$project$Main$modernSansSerif,
																				$elm$core$String$isEmpty(
																				$elm$core$String$trim(model.addPlayerName)) ? $rtfeldman$elm_css$Css$batch(
																				_List_fromArray(
																					[
																						$rtfeldman$elm_css$Css$opacity(
																						$rtfeldman$elm_css$Css$num(0.5)),
																						$rtfeldman$elm_css$Css$cursor($rtfeldman$elm_css$Css$notAllowed)
																					])) : $rtfeldman$elm_css$Css$batch(_List_Nil)
																			])),
																		$elm$core$String$isEmpty(
																		$elm$core$String$trim(model.addPlayerName)) ? $rtfeldman$elm_css$Html$Styled$Attributes$disabled(true) : $rtfeldman$elm_css$Html$Styled$Events$onClick($author$project$Main$KeeperConfirmedAddPlayer)
																	]),
																_List_fromArray(
																	[
																		$rtfeldman$elm_css$Html$Styled$text('Add Player')
																	]))
															]))
													]))
											]))
									]) : _List_Nil,
								function () {
									var _v4 = model.status;
									if (_v4.$ === 'Just') {
										var message = _v4.a;
										return _List_fromArray(
											[
												A2(
												$rtfeldman$elm_css$Html$Styled$div,
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$Attributes$css(
														_List_fromArray(
															[
																$rtfeldman$elm_css$Css$position($rtfeldman$elm_css$Css$fixed),
																$rtfeldman$elm_css$Css$top(
																$rtfeldman$elm_css$Css$px(20)),
																$rtfeldman$elm_css$Css$right(
																$rtfeldman$elm_css$Css$px(20)),
																$rtfeldman$elm_css$Css$backgroundColor(
																model.autoSaveInProgress ? $rtfeldman$elm_css$Css$hex('EF4444') : $rtfeldman$elm_css$Css$hex('10B981')),
																$rtfeldman$elm_css$Css$color(
																$rtfeldman$elm_css$Css$hex('FFFFFF')),
																A4(
																$rtfeldman$elm_css$Css$padding4,
																$rtfeldman$elm_css$Css$px(12),
																$rtfeldman$elm_css$Css$px(16),
																$rtfeldman$elm_css$Css$px(12),
																$rtfeldman$elm_css$Css$px(16)),
																$rtfeldman$elm_css$Css$borderRadius(
																$rtfeldman$elm_css$Css$px(8)),
																A4(
																$rtfeldman$elm_css$Css$boxShadow4,
																$rtfeldman$elm_css$Css$px(0),
																$rtfeldman$elm_css$Css$px(4),
																$rtfeldman$elm_css$Css$px(12),
																A4($rtfeldman$elm_css$Css$rgba, 0, 0, 0, 0.15)),
																A3(
																$rtfeldman$elm_css$Css$border3,
																$rtfeldman$elm_css$Css$px(1),
																$rtfeldman$elm_css$Css$solid,
																A4($rtfeldman$elm_css$Css$rgba, 255, 255, 255, 0.2)),
																A2($rtfeldman$elm_css$Css$property, 'animation', 'slideInFromRight 0.3s ease-out'),
																$rtfeldman$elm_css$Css$fontSize(
																$rtfeldman$elm_css$Css$px(14)),
																$rtfeldman$elm_css$Css$fontWeight(
																$rtfeldman$elm_css$Css$int(500)),
																$rtfeldman$elm_css$Css$maxWidth(
																$rtfeldman$elm_css$Css$px(300)),
																$rtfeldman$elm_css$Css$zIndex(
																$rtfeldman$elm_css$Css$int(1000)),
																$author$project$Main$modernSansSerif
															]))
													]),
												_List_fromArray(
													[
														A2(
														$rtfeldman$elm_css$Html$Styled$div,
														_List_fromArray(
															[
																$rtfeldman$elm_css$Html$Styled$Attributes$css(
																_List_fromArray(
																	[
																		$rtfeldman$elm_css$Css$displayFlex,
																		$rtfeldman$elm_css$Css$alignItems($rtfeldman$elm_css$Css$center),
																		$rtfeldman$elm_css$Css$justifyContent($rtfeldman$elm_css$Css$spaceBetween)
																	]))
															]),
														_List_fromArray(
															[
																A2(
																$rtfeldman$elm_css$Html$Styled$span,
																_List_Nil,
																_List_fromArray(
																	[
																		$rtfeldman$elm_css$Html$Styled$text(
																		_Utils_ap(
																			message,
																			model.autoSaveInProgress ? ' (Voting disabled - autoSave)' : (model.isSyncing ? ' (Voting disabled - syncing)' : ' (Voting enabled)')))
																	])),
																A2(
																$rtfeldman$elm_css$Html$Styled$button,
																_List_fromArray(
																	[
																		$rtfeldman$elm_css$Html$Styled$Attributes$css(
																		_List_fromArray(
																			[
																				$rtfeldman$elm_css$Css$backgroundColor($rtfeldman$elm_css$Css$transparent),
																				$rtfeldman$elm_css$Css$border($rtfeldman$elm_css$Css$zero),
																				$rtfeldman$elm_css$Css$color(
																				$rtfeldman$elm_css$Css$hex('FFFFFF')),
																				$rtfeldman$elm_css$Css$cursor($rtfeldman$elm_css$Css$pointer),
																				$rtfeldman$elm_css$Css$fontSize(
																				$rtfeldman$elm_css$Css$px(18)),
																				$rtfeldman$elm_css$Css$fontWeight(
																				$rtfeldman$elm_css$Css$int(400)),
																				$rtfeldman$elm_css$Css$marginLeft(
																				$rtfeldman$elm_css$Css$px(12)),
																				$rtfeldman$elm_css$Css$padding($rtfeldman$elm_css$Css$zero),
																				$rtfeldman$elm_css$Css$opacity(
																				$rtfeldman$elm_css$Css$num(0.8)),
																				$rtfeldman$elm_css$Css$hover(
																				_List_fromArray(
																					[
																						$rtfeldman$elm_css$Css$opacity(
																						$rtfeldman$elm_css$Css$num(1.0))
																					]))
																			])),
																		$rtfeldman$elm_css$Html$Styled$Events$onClick($author$project$Main$ClearStatus)
																	]),
																_List_fromArray(
																	[
																		$rtfeldman$elm_css$Html$Styled$text('×')
																	]))
															]))
													]))
											]);
									} else {
										return _List_Nil;
									}
								}())))))),
		title: 'Hockey Rater 🏒'
	};
};
var $author$project$Main$main = $elm$browser$Browser$document(
	{init: $author$project$Main$init, subscriptions: $author$project$Main$subscriptions, update: $author$project$Main$update, view: $author$project$Main$view});
_Platform_export({'Main':{'init':$author$project$Main$main(
	$elm$json$Json$Decode$succeed(
		{}))(0)}});}(this));