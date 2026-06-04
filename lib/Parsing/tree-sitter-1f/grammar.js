module.exports = grammar({
  name: 'odin',

  word: $ => $.lower_identifier,

  extras: $ => [
    /\s/,
    /\/\/[^\n]*/,
    /\/\*[\s\S]*?\*\//,
  ],

  conflicts: $ => [
    [$.binop, $.binop],
    [$.pat_ctor, $.pat_ctor],
    [$.pat_unit,$.unit_literal],
    [$.tuple_or_paren, $.unit_literal],
    [$.match_expr,$.match_expr]
  ],

  rules: {
    program: $ => repeat($.decl),

    decl: $ => choice(
      $.let_decl,
      $.let_rec_group,
      $.module_decl,
      $.alias_decl,
      $.adt_decl,
      $.record_decl
    ),

    let_decl: $ => seq(
      'пусть',
      field('name', $.lower_identifier),
      repeat($.pattern_arg),
      optional(seq(':', field('typ', $.typ))),
      '=',
      field('body', $.expr)
    ),

    let_rec_group: $ => seq(
      'пусть', 'рек',
      field('first', $.let_binding),
      repeat(seq('и', field('next', $.let_binding)))
    ),

    let_binding: $ => seq(
      field('name', $.pattern),
      repeat($.pattern_arg),
      optional(seq(':', field('typ', $.typ))),
      '=',
      field('body', $.expr)
    ),

    module_decl: $ => seq(
      'модуль',
      field('name', $.upper_identifier),
      '=',
      'структура',
      repeat($.decl),
      'конец'
    ),

    alias_decl: $ => seq(
      'алиас',
      field('name', $.upper_identifier),
      optional($.generic_vars),
      '=',
      field('typ', $.typ)
    ),

    adt_decl: $ => seq(
      'тип',
      field('name', $.upper_identifier),
      optional($.generic_vars),
      '=',
      optional('|'),
      sep1($.constructor, '|')
    ),

    constructor: $ => choice(
      seq(field('name', $.upper_identifier), 'из', field('typ', $.typ)),
      field('name', $.upper_identifier)
    ),

    record_decl: $ => seq(
      'тип',
      field('name', $.upper_identifier),
      optional($.generic_vars),
      '=',
      '{', sepBy($.record_field, ';'), '}'
    ),

    record_field: $ => seq(
      field('name', $.lower_identifier),
      ':',
      field('typ', $.typ)
    ),

    generic_vars: $ => seq('<', sep1($.type_var, ','), '>'),
    type_var: $ => $.lower_identifier,

    pattern_arg: $ => choice(
      $.typed_pattern,
      field('pattern', $.pattern)
    ),

    typed_pattern: $ => seq('(', field('pattern', $.pattern), ':', field('typ', $.typ), ')'),

    pattern: $ => choice(
      $.pat_unit,
      $.pat_variable,
      $.pat_tuple,
      $.pat_ctor,
      $.pat_wildcard,
      $.pat_list_cons,
      $.pat_literal,
      $.pat_empty_list
    ),

    pat_unit: $ => seq('(', ')'),
    pat_variable: $ => field('name', $.lower_identifier),
    pat_wildcard: $ => '_',
    pat_empty_list: $ => seq('[', ']'),
    
    pat_tuple: $ => seq('(', sep1($.pattern, ','), ')'),
    
    pat_ctor: $ => seq(
      field('name', $.upper_identifier),
      optional(field('arg', $.pattern))
    ),
    
    pat_list_cons: $ => prec.right(1, seq(
      field('head', $.pattern),
      '::',
      field('tail', $.pattern)
    )),
    
    pat_literal: $ => $.literal,

    literal: $ => choice(
      $.int_literal,
      $.float_literal,
      $.string_literal,
      $.bool_literal,
      $.char_literal,
      $.unit_literal
    ),

    unit_literal: $ => seq('(', ')'),
    int_literal: $ => /[0-9]+/,
    float_literal: $ => /[0-9]+\.[0-9]+/,
    string_literal: $ => /"[^"]*"/,
    char_literal: $ => /'[^']*'/,
    bool_literal: $ => choice('да', 'нет'),

    typ: $ => choice(
      $.typ_ground,
      $.typ_var,
      $.typ_arrow,
      $.typ_tuple,
      $.typ_ctor
    ),

    typ_ground: $ => choice('скиб', 'символ', 'строка', 'инт', 'бул', 'дроб'),
    typ_var: $ => $.lower_identifier,
    typ_arrow: $ => prec.right(1, seq(field('left', $.typ), '->', field('right', $.typ))),
    typ_tuple: $ => seq('(', sep1($.typ, ','), ')'),
    typ_ctor: $ => seq(field('name', $.upper_identifier), optional(seq('<', sep1($.typ, ','), '>'))),

    expr: $ => choice(
      $.let_expr,
      $.if_expr,
      $.lambda_expr,
      $.match_expr,
      $.binop
    ),

    let_expr: $ => seq(
      'пусть',
      optional('рек'),
      field('pattern', $.pattern),
      repeat($.pattern_arg),
      optional(seq(':', field('typ', $.typ))),
      '=',
      field('bound', $.expr),
      'в',
      field('body', $.expr)
    ),

    if_expr: $ => seq(
      'если',
      field('cond', $.expr),
      'то',
      field('then', $.expr),
      'иначе',
      field('else', $.expr)
    ),

    lambda_expr: $ => seq(
      'лямбда',
      field('arg', $.typed_pattern),
      '->',
      field('body', $.expr)
    ),

    match_expr: $ => seq(
      'сопоставить',
      field('match', $.expr),
      'с',
      optional('|'),
      sep1($.match_branch, '|')
    ),

    match_branch: $ => seq(
      field('pattern', $.pattern),
      optional(seq('когда', field('when', $.expr))),
      '->',
      field('result', $.expr)
    ),

    binop: $ => choice(
      $.application,
      prec.left(1, seq($.binop, ';', $.binop)),
      prec.left(2, seq($.binop, ',', $.binop)),
      prec.left(3, seq($.binop, '|', $.binop)),
      prec.left(4, seq($.binop, '&', $.binop)),
      prec.left(5, seq($.binop, choice('<=', '>=', '<>', '>>=', '>>', '<<', '<', '>', '='), $.binop)),
      prec.right(6, seq($.binop, choice('::', '^', '@'), $.binop)),
      prec.right(7, seq($.binop, '|>', $.binop)),
      prec.right(8, seq($.binop, '>>=', $.binop)),
      prec.left(9, seq($.binop, '+', $.binop)),
      prec.left(9, seq($.binop, '-', $.binop)),
      prec.left(10, seq($.binop, '*', $.binop)),
      prec.left(10, seq($.binop, '/', $.binop)),
      prec(11, seq($.operator_prefix, $.binop))
    ),

    operator_prefix: $ => choice('!', '~', '#', '-', 'не'),

    application: $ => prec.left(12, seq(
      field('function', $.application),
      field('argument', $.atom)
    )),

    atom: $ => choice(
      $.literal,
      $.value,
      $.ctor,
      $.tuple_or_paren,
      $.record_init,
      $.record_update,
      $.field_access,
      $.list_expr,
      $.lambda_expr,
      $.if_expr
    ),

    tuple_or_paren: $ => choice(
      seq('(', sep1($.expr, ','), ')'),
      seq('(', $.expr, ')'),
      seq('(', ')')
    ),

    value: $ => $.lower_identifier,
    ctor: $ => $.upper_identifier,

    list_expr: $ => seq('[', sepBy($.expr, ','), ']'),

    record_init: $ => seq('{', sepBy($.record_binding, ','), '}'),
    record_update: $ => seq('{', $.expr, 'с', sepBy($.record_binding, ','), '}'),
    record_binding: $ => seq(field('field', $.lower_identifier), '=', field('value', $.expr)),

    field_access: $ => prec.left(13, seq(
      field('record', $.field_access),
      '.',
      field('field', $.lower_identifier)
    )),

    lower_identifier: $ => /[а-яё][а-яё0-9_]*/u,
    upper_identifier: $ => /[А-ЯЁ][а-яё0-9_]*/u,
  }
});

function sep1(rule, separator) {
  return seq(rule, repeat(seq(separator, rule)));
}

function sepBy(rule, separator) {
  return optional(sep1(rule, separator));
}