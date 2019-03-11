// A Bison parser, made by GNU Bison 3.0.4.

// Skeleton implementation for Bison LALR(1) parsers in C++

// Copyright (C) 2002-2015 Free Software Foundation, Inc.

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

// As a special exception, you may create a larger work that contains
// part or all of the Bison parser skeleton and distribute that work
// under terms of your choice, so long as that work isn't itself a
// parser generator using the skeleton or a modified version thereof
// as a parser skeleton.  Alternatively, if you modify or redistribute
// the parser skeleton itself, you may (at your option) remove this
// special exception, which will cause the skeleton and the resulting
// Bison output files to be licensed under the GNU General Public
// License without this special exception.

// This special exception was added by the Free Software Foundation in
// version 2.2 of Bison.


// First part of user declarations.

#line 37 "conj.c" // lalr1.cc:404

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

// //                    "%code requires" blocks.
#line 10 "GrammarConfig.y" // lalr1.cc:408

#include <map>
#include <list>
#include <vector>
#include <string>
#include <iostream>
#include <algorithm>

#define ENUM_IDENTIFIERS(o) \
		o(undefine)					/*undefined*/ \
		o(function)					/*pointer to a function \
		o(parameter)				/*parameter to a function*/ \
		o(variable)					/*local variables*/
#define o(n) n,
enum class id_type	{ ENUM_IDENTIFIERS(o) };
#undef o

struct identifier
{
	id_type type =	id_type::undefined;
	std::size_t		index = 0;	// function#, parameter# within function, variable#
	std::string		name;
}

#define ENUM_EXPRESSIONS(o) \
		o(nop) o(string) o(number) o(ident)		/*atoms*/ \
		o(add) o(neg) o(eq)						/*transformations*/ \
		o(cor) o(cand) o(loop)					/*logic - a loop looks like: while(param0) { param1...n }*/ \
		o(addrof) o(deref)						/*pointers*/ \
		o(fcall)								/*function call*/ \
		o(copy)									/*assignment operator*/ \
		o(comma)								/*expression sequence*/ \
		o(ret)									/*return(param0)*/ \

#define o(n) n,
enum class ex_type { ENUM_EXPRESSIONS(o) };
#undef o

typedef std::list(struct expression) expr_vec;
struct expression
{
	ex_type			type;
	identifier		ident{};		//For identifiers
	std::string		strvalue{};		//For strings
	long			numvalue=0;		//For numbers
	expr_vec		params;
	//For while() and if(), the first item is the condition, the rest is contingent statements
	//for fcall, the first parameter is the function to call

	template<typename... T>
	expression(ex_type t, T&&... args) : type(t), params{std::forward<T>(args)... } { }

	expression()					:	type(ex_type::nop) { }
	expression(const identifier &i)	:	type(ex_type::ident),	ident(i) { }
	expression(identifier &&i)		:	type(ex_type::ident),	ident(std::move(i)) { }
	expression(std::string &&s)		:	type(ex_type::string),	strvalue(std::move(s)) { }
	expression(long v)				:	type(ex_type::number),	numvalue(v) { }

	bool is_pure() const;

	expression operator%=(expression && b) && { return expression(ex_type::copy, std::move(b), std::move(*this)); }
};

#define o(n) \
template<typename... T> \
inline expression e_##n(T&&... args) { return expression(ex_type::n, std::forward<T>(args)...); }
ENUM_EXPRESSIONS(o)
#undef o

struct function
{
	std::string name;
	expression code;
	unsigned num_vars = 0, num_params = 0;
};

struct lexcontext;


#line 128 "conj.c" // lalr1.cc:408

# include <cassert>
# include <cstdlib> // std::abort
# include <iostream>
# include <stdexcept>
# include <string>
# include <vector>
#include <typeinfo>
#ifndef YYASSERT
# include <cassert>
# define YYASSERT assert
#endif


#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif


namespace yy {
#line 203 "conj.c" // lalr1.cc:408

  template <class T, class S = std::vector<T> >
  class stack
  {
  public:
    // Hide our reversed order.
    typedef typename S::reverse_iterator iterator;
    typedef typename S::const_reverse_iterator const_iterator;

    stack ()
      : seq_ ()
    {
      seq_.reserve (200);
    }

    stack (unsigned int n)
      : seq_ (n)
    {}

    inline
    T&
    operator[] (unsigned int i)
    {
      return seq_[seq_.size () - 1 - i];
    }

    inline
    const T&
    operator[] (unsigned int i) const
    {
      return seq_[seq_.size () - 1 - i];
    }

    /// Steal the contents of \a t.
    ///
    /// Close to move-semantics.
    inline
    void
    push (T& t)
    {
      seq_.push_back (T());
      operator[](0).move (t);
    }

    inline
    void
    pop (unsigned int n = 1)
    {
      for (; n; --n)
        seq_.pop_back ();
    }

    void
    clear ()
    {
      seq_.clear ();
    }

    inline
    typename S::size_type
    size () const
    {
      return seq_.size ();
    }

    inline
    const_iterator
    begin () const
    {
      return seq_.rbegin ();
    }

    inline
    const_iterator
    end () const
    {
      return seq_.rend ();
    }

  private:
    stack (const stack&);
    stack& operator= (const stack&);
    /// The wrapped container.
    S seq_;
  };

  /// Present a slice of the top of a stack.
  template <class T, class S = stack<T> >
  class slice
  {
  public:
    slice (const S& stack, unsigned int range)
      : stack_ (stack)
      , range_ (range)
    {}

    inline
    const T&
    operator [] (unsigned int i) const
    {
      return stack_[range_ - i];
    }

  private:
    const S& stack_;
    unsigned int range_;
  };

  /// Abstract a position.
  class position
  {
  public:
    /// Construct a position.
    explicit position (std::string* f = YY_NULLPTR,
                       unsigned int l = 1u,
                       unsigned int c = 1u)
      : filename (f)
      , line (l)
      , column (c)
    {
    }


    /// Initialization.
    void initialize (std::string* fn = YY_NULLPTR,
                     unsigned int l = 1u,
                     unsigned int c = 1u)
    {
      filename = fn;
      line = l;
      column = c;
    }

    /** \name Line and Column related manipulators
     ** \{ */
    /// (line related) Advance to the COUNT next lines.
    void lines (int count = 1)
    {
      if (count)
        {
          column = 1u;
          line = add_ (line, count, 1);
        }
    }

    /// (column related) Advance to the COUNT next columns.
    void columns (int count = 1)
    {
      column = add_ (column, count, 1);
    }
    /** \} */

    /// File name to which this position refers.
    std::string* filename;
    /// Current line number.
    unsigned int line;
    /// Current column number.
    unsigned int column;

  private:
    /// Compute max(min, lhs+rhs) (provided min <= lhs).
    static unsigned int add_ (unsigned int lhs, int rhs, unsigned int min)
    {
      return (0 < rhs || -static_cast<unsigned int>(rhs) < lhs
              ? rhs + lhs
              : min);
    }
  };

  /// Add \a width columns, in place.
  inline position&
  operator+= (position& res, int width)
  {
    res.columns (width);
    return res;
  }

  /// Add \a width columns.
  inline position
  operator+ (position res, int width)
  {
    return res += width;
  }

  /// Subtract \a width columns, in place.
  inline position&
  operator-= (position& res, int width)
  {
    return res += -width;
  }

  /// Subtract \a width columns.
  inline position
  operator- (position res, int width)
  {
    return res -= width;
  }

  /// Compare two position objects.
  inline bool
  operator== (const position& pos1, const position& pos2)
  {
    return (pos1.line == pos2.line
            && pos1.column == pos2.column
            && (pos1.filename == pos2.filename
                || (pos1.filename && pos2.filename
                    && *pos1.filename == *pos2.filename)));
  }

  /// Compare two position objects.
  inline bool
  operator!= (const position& pos1, const position& pos2)
  {
    return !(pos1 == pos2);
  }

  /** \brief Intercept output stream redirection.
   ** \param ostr the destination output stream
   ** \param pos a reference to the position to redirect
   */
  template <typename YYChar>
  inline std::basic_ostream<YYChar>&
  operator<< (std::basic_ostream<YYChar>& ostr, const position& pos)
  {
    if (pos.filename)
      ostr << *pos.filename << ':';
    return ostr << pos.line << '.' << pos.column;
  }

  /// Abstract a location.
  class location
  {
  public:

    /// Construct a location from \a b to \a e.
    location (const position& b, const position& e)
      : begin (b)
      , end (e)
    {
    }

    /// Construct a 0-width location in \a p.
    explicit location (const position& p = position ())
      : begin (p)
      , end (p)
    {
    }

    /// Construct a 0-width location in \a f, \a l, \a c.
    explicit location (std::string* f,
                       unsigned int l = 1u,
                       unsigned int c = 1u)
      : begin (f, l, c)
      , end (f, l, c)
    {
    }


    /// Initialization.
    void initialize (std::string* f = YY_NULLPTR,
                     unsigned int l = 1u,
                     unsigned int c = 1u)
    {
      begin.initialize (f, l, c);
      end = begin;
    }

    /** \name Line and Column related manipulators
     ** \{ */
  public:
    /// Reset initial location to final location.
    void step ()
    {
      begin = end;
    }

    /// Extend the current location to the COUNT next columns.
    void columns (int count = 1)
    {
      end += count;
    }

    /// Extend the current location to the COUNT next lines.
    void lines (int count = 1)
    {
      end.lines (count);
    }
    /** \} */


  public:
    /// Beginning of the located region.
    position begin;
    /// End of the located region.
    position end;
  };

  /// Join two locations, in place.
  inline location& operator+= (location& res, const location& end)
  {
    res.end = end.end;
    return res;
  }

  /// Join two locations.
  inline location operator+ (location res, const location& end)
  {
    return res += end;
  }

  /// Add \a width columns to the end position, in place.
  inline location& operator+= (location& res, int width)
  {
    res.columns (width);
    return res;
  }

  /// Add \a width columns to the end position.
  inline location operator+ (location res, int width)
  {
    return res += width;
  }

  /// Subtract \a width columns to the end position, in place.
  inline location& operator-= (location& res, int width)
  {
    return res += -width;
  }

  /// Subtract \a width columns to the end position.
  inline location operator- (location res, int width)
  {
    return res -= width;
  }

  /// Compare two location objects.
  inline bool
  operator== (const location& loc1, const location& loc2)
  {
    return loc1.begin == loc2.begin && loc1.end == loc2.end;
  }

  /// Compare two location objects.
  inline bool
  operator!= (const location& loc1, const location& loc2)
  {
    return !(loc1 == loc2);
  }

  /** \brief Intercept output stream redirection.
   ** \param ostr the destination output stream
   ** \param loc a reference to the location to redirect
   **
   ** Avoid duplicate information.
   */
  template <typename YYChar>
  inline std::basic_ostream<YYChar>&
  operator<< (std::basic_ostream<YYChar>& ostr, const location& loc)
  {
    unsigned int end_col = 0 < loc.end.column ? loc.end.column - 1 : 0;
    ostr << loc.begin;
    if (loc.end.filename
        && (!loc.begin.filename
            || *loc.begin.filename != *loc.end.filename))
      ostr << '-' << loc.end.filename << ':' << loc.end.line << '.' << end_col;
    else if (loc.begin.line < loc.end.line)
      ostr << '-' << loc.end.line << '.' << end_col;
    else if (loc.begin.column < end_col)
      ostr << '-' << end_col;
    return ostr;
  }


  /// A char[S] buffer to store and retrieve objects.
  ///
  /// Sort of a variant, but does not keep track of the nature
  /// of the stored data, since that knowledge is available
  /// via the current state.
  template <size_t S>
  struct variant
  {
    /// Type of *this.
    typedef variant<S> self_type;

    /// Empty construction.
    variant ()
      : yytypeid_ (YY_NULLPTR)
    {}

    /// Construct and fill.
    template <typename T>
    variant (const T& t)
      : yytypeid_ (&typeid (T))
    {
      YYASSERT (sizeof (T) <= S);
      new (yyas_<T> ()) T (t);
    }

    /// Destruction, allowed only if empty.
    ~variant ()
    {
      YYASSERT (!yytypeid_);
    }

    /// Instantiate an empty \a T in here.
    template <typename T>
    T&
    build ()
    {
      YYASSERT (!yytypeid_);
      YYASSERT (sizeof (T) <= S);
      yytypeid_ = & typeid (T);
      return *new (yyas_<T> ()) T;
    }

    /// Instantiate a \a T in here from \a t.
    template <typename T>
    T&
    build (const T& t)
    {
      YYASSERT (!yytypeid_);
      YYASSERT (sizeof (T) <= S);
      yytypeid_ = & typeid (T);
      return *new (yyas_<T> ()) T (t);
    }

    /// Accessor to a built \a T.
    template <typename T>
    T&
    as ()
    {
      YYASSERT (*yytypeid_ == typeid (T));
      YYASSERT (sizeof (T) <= S);
      return *yyas_<T> ();
    }

    /// Const accessor to a built \a T (for %printer).
    template <typename T>
    const T&
    as () const
    {
      YYASSERT (*yytypeid_ == typeid (T));
      YYASSERT (sizeof (T) <= S);
      return *yyas_<T> ();
    }

    /// Swap the content with \a other, of same type.
    ///
    /// Both variants must be built beforehand, because swapping the actual
    /// data requires reading it (with as()), and this is not possible on
    /// unconstructed variants: it would require some dynamic testing, which
    /// should not be the variant's responsability.
    /// Swapping between built and (possibly) non-built is done with
    /// variant::move ().
    template <typename T>
    void
    swap (self_type& other)
    {
      YYASSERT (yytypeid_);
      YYASSERT (*yytypeid_ == *other.yytypeid_);
      std::swap (as<T> (), other.as<T> ());
    }

    /// Move the content of \a other to this.
    ///
    /// Destroys \a other.
    template <typename T>
    void
    move (self_type& other)
    {
      build<T> ();
      swap<T> (other);
      other.destroy<T> ();
    }

    /// Copy the content of \a other to this.
    template <typename T>
    void
    copy (const self_type& other)
    {
      build<T> (other.as<T> ());
    }

    /// Destroy the stored \a T.
    template <typename T>
    void
    destroy ()
    {
      as<T> ().~T ();
      yytypeid_ = YY_NULLPTR;
    }

  private:
    /// Prohibit blind copies.
    self_type& operator=(const self_type&);
    variant (const self_type&);

    /// Accessor to raw memory as \a T.
    template <typename T>
    T*
    yyas_ ()
    {
      void *yyp = yybuffer_.yyraw;
      return static_cast<T*> (yyp);
     }

    /// Const accessor to raw memory as \a T.
    template <typename T>
    const T*
    yyas_ () const
    {
      const void *yyp = yybuffer_.yyraw;
      return static_cast<const T*> (yyp);
     }

    union
    {
      /// Strongest alignment constraints.
      long double yyalign_me;
      /// A buffer large enough to store any of the semantic values.
      char yyraw[S];
    } yybuffer_;

    /// Whether the content is built: if defined, the name of the stored type.
    const std::type_info *yytypeid_;
  };


  /// A Bison parser.
  class conj_parser
  {
  public:
#ifndef YYSTYPE
    /// An auxiliary type to compute the largest semantic type.
    union union_type
    {
      // stmt
      // com_stmt
      // var_defs
      // var_def1
      // exprs
      // c_expr1
      // expr
      char dummy1[sizeof(expression)];

      // NUMCONST
      char dummy2[sizeof(long)];

      // IDENTIFIER
      // STRINGCONST
      char dummy3[sizeof(std::string)];
};

    /// Symbol semantic values.
    typedef variant<sizeof(union_type)> semantic_type;
#else
    typedef YYSTYPE semantic_type;
#endif
    /// Symbol locations.
    typedef location location_type;

    /// Syntax errors thrown from user actions.
    struct syntax_error : std::runtime_error
    {
      syntax_error (const location_type& l, const std::string& m);
      location_type location;
    };

    /// Tokens.
    struct token
    {
      enum yytokentype
      {
        END = 0,
        RETURN = 258,
        WHILE = 259,
        IF = 260,
        VAR = 261,
        IDENTIFIER = 262,
        NUMCONST = 263,
        STRINGCONST = 264,
        OR = 265,
        AND = 266,
        EQ = 267,
        NE = 268,
        PP = 269,
        MM = 270,
        PL_EQ = 271,
        MI_EQ = 272
      };
    };

    /// (External) token type, as returned by yylex.
    typedef token::yytokentype token_type;

    /// Symbol type: an internal symbol number.
    typedef int symbol_number_type;

    /// The symbol type number to denote an empty symbol.
    enum { empty_symbol = -2 };

    /// Internal symbol number for tokens (subsumed by symbol_number_type).
    typedef unsigned char token_number_type;

    /// A complete symbol.
    ///
    /// Expects its Base type to provide access to the symbol type
    /// via type_get().
    ///
    /// Provide access to semantic value and location.
    template <typename Base>
    struct basic_symbol : Base
    {
      /// Alias to Base.
      typedef Base super_type;

      /// Default constructor.
      basic_symbol ();

      /// Copy constructor.
      basic_symbol (const basic_symbol& other);

      /// Constructor for valueless symbols, and symbols from each type.

  basic_symbol (typename Base::kind_type t, const location_type& l);

  basic_symbol (typename Base::kind_type t, const expression v, const location_type& l);

  basic_symbol (typename Base::kind_type t, const long v, const location_type& l);

  basic_symbol (typename Base::kind_type t, const std::string v, const location_type& l);


      /// Constructor for symbols with semantic value.
      basic_symbol (typename Base::kind_type t,
                    const semantic_type& v,
                    const location_type& l);

      /// Destroy the symbol.
      ~basic_symbol ();

      /// Destroy contents, and record that is empty.
      void clear ();

      /// Whether empty.
      bool empty () const;

      /// Destructive move, \a s is emptied into this.
      void move (basic_symbol& s);

      /// The semantic value.
      semantic_type value;

      /// The location.
      location_type location;

    private:
      /// Assignment operator.
      basic_symbol& operator= (const basic_symbol& other);
    };

    /// Type access provider for token (enum) based symbols.
    struct by_type
    {
      /// Default constructor.
      by_type ();

      /// Copy constructor.
      by_type (const by_type& other);

      /// The symbol type as needed by the constructor.
      typedef token_type kind_type;

      /// Constructor from (external) token numbers.
      by_type (kind_type t);

      /// Record that this symbol is empty.
      void clear ();

      /// Steal the symbol type from \a that.
      void move (by_type& that);

      /// The (internal) type number (corresponding to \a type).
      /// \a empty when empty.
      symbol_number_type type_get () const;

      /// The token.
      token_type token () const;

      /// The symbol type.
      /// \a empty_symbol when empty.
      /// An int, not token_number_type, to be able to store empty_symbol.
      int type;
    };

    /// "External" symbols: returned by the scanner.
    typedef basic_symbol<by_type> symbol_type;

    // Symbol constructors declarations.
    static inline
    symbol_type
    make_END (const location_type& l);

    static inline
    symbol_type
    make_RETURN (const location_type& l);

    static inline
    symbol_type
    make_WHILE (const location_type& l);

    static inline
    symbol_type
    make_IF (const location_type& l);

    static inline
    symbol_type
    make_VAR (const location_type& l);

    static inline
    symbol_type
    make_IDENTIFIER (const std::string& v, const location_type& l);

    static inline
    symbol_type
    make_NUMCONST (const long& v, const location_type& l);

    static inline
    symbol_type
    make_STRINGCONST (const std::string& v, const location_type& l);

    static inline
    symbol_type
    make_OR (const location_type& l);

    static inline
    symbol_type
    make_AND (const location_type& l);

    static inline
    symbol_type
    make_EQ (const location_type& l);

    static inline
    symbol_type
    make_NE (const location_type& l);

    static inline
    symbol_type
    make_PP (const location_type& l);

    static inline
    symbol_type
    make_MM (const location_type& l);

    static inline
    symbol_type
    make_PL_EQ (const location_type& l);

    static inline
    symbol_type
    make_MI_EQ (const location_type& l);


    /// Build a parser object.
    conj_parser (lexcontext &ctx_yyarg);
    virtual ~conj_parser ();

    /// Parse.
    /// \returns  0 iff parsing succeeded.
    virtual int parse ();

#if YYDEBUG
    /// The current debugging stream.
    std::ostream& debug_stream () const YY_ATTRIBUTE_PURE;
    /// Set the current debugging stream.
    void set_debug_stream (std::ostream &);

    /// Type for debugging levels.
    typedef int debug_level_type;
    /// The current debugging level.
    debug_level_type debug_level () const YY_ATTRIBUTE_PURE;
    /// Set the current debugging level.
    void set_debug_level (debug_level_type l);
#endif

    /// Report a syntax error.
    /// \param loc    where the syntax error is found.
    /// \param msg    a description of the syntax error.
    virtual void error (const location_type& loc, const std::string& msg);

    /// Report a syntax error.
    void error (const syntax_error& err);

  private:
    /// This class is not copyable.
    conj_parser (const conj_parser&);
    conj_parser& operator= (const conj_parser&);

    /// State numbers.
    typedef int state_type;

    /// Generate an error message.
    /// \param yystate   the state where the error occurred.
    /// \param yyla      the lookahead token.
    virtual std::string yysyntax_error_ (state_type yystate,
                                         const symbol_type& yyla) const;

    /// Compute post-reduction state.
    /// \param yystate   the current state
    /// \param yysym     the nonterminal to push on the stack
    state_type yy_lr_goto_state_ (state_type yystate, int yysym);

    /// Whether the given \c yypact_ value indicates a defaulted state.
    /// \param yyvalue   the value to check
    static bool yy_pact_value_is_default_ (int yyvalue);

    /// Whether the given \c yytable_ value indicates a syntax error.
    /// \param yyvalue   the value to check
    static bool yy_table_value_is_error_ (int yyvalue);

    static const signed char yypact_ninf_;
    static const signed char yytable_ninf_;

    /// Convert a scanner token number \a t to a symbol number.
    static token_number_type yytranslate_ (token_type t);

    // Tables.
  // YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
  // STATE-NUM.
  static const short int yypact_[];

  // YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
  // Performed when YYTABLE does not specify something else to do.  Zero
  // means the default is an error.
  static const unsigned char yydefact_[];

  // YYPGOTO[NTERM-NUM].
  static const signed char yypgoto_[];

  // YYDEFGOTO[NTERM-NUM].
  static const signed char yydefgoto_[];

  // YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
  // positive, shift that token.  If negative, reduce the rule whose
  // number is the opposite.  If YYTABLE_NINF, syntax error.
  static const unsigned char yytable_[];

  static const signed char yycheck_[];

  // YYSTOS[STATE-NUM] -- The (internal number of the) accessing
  // symbol of state STATE-NUM.
  static const unsigned char yystos_[];

  // YYR1[YYN] -- Symbol number of symbol that rule YYN derives.
  static const unsigned char yyr1_[];

  // YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.
  static const unsigned char yyr2_[];


    /// Convert the symbol name \a n to a form suitable for a diagnostic.
    static std::string yytnamerr_ (const char *n);


    /// For a symbol, its name in clear.
    static const char* const yytname_[];
#if YYDEBUG
  // YYRLINE[YYN] -- Source line where rule number YYN was defined.
  static const unsigned char yyrline_[];
    /// Report on the debug stream that the rule \a r is going to be reduced.
    virtual void yy_reduce_print_ (int r);
    /// Print the state stack on the debug stream.
    virtual void yystack_print_ ();

    // Debugging.
    int yydebug_;
    std::ostream* yycdebug_;

    /// \brief Display a symbol type, value and location.
    /// \param yyo    The output stream.
    /// \param yysym  The symbol.
    template <typename Base>
    void yy_print_ (std::ostream& yyo, const basic_symbol<Base>& yysym) const;
#endif

    /// \brief Reclaim the memory associated to a symbol.
    /// \param yymsg     Why this token is reclaimed.
    ///                  If null, print nothing.
    /// \param yysym     The symbol.
    template <typename Base>
    void yy_destroy_ (const char* yymsg, basic_symbol<Base>& yysym) const;

  private:
    /// Type access provider for state based symbols.
    struct by_state
    {
      /// Default constructor.
      by_state ();

      /// The symbol type as needed by the constructor.
      typedef state_type kind_type;

      /// Constructor.
      by_state (kind_type s);

      /// Copy constructor.
      by_state (const by_state& other);

      /// Record that this symbol is empty.
      void clear ();

      /// Steal the symbol type from \a that.
      void move (by_state& that);

      /// The (internal) type number (corresponding to \a state).
      /// \a empty_symbol when empty.
      symbol_number_type type_get () const;

      /// The state number used to denote an empty symbol.
      enum { empty_state = -1 };

      /// The state.
      /// \a empty when empty.
      state_type state;
    };

    /// "Internal" symbol: element of the stack.
    struct stack_symbol_type : basic_symbol<by_state>
    {
      /// Superclass.
      typedef basic_symbol<by_state> super_type;
      /// Construct an empty symbol.
      stack_symbol_type ();
      /// Steal the contents from \a sym to build this.
      stack_symbol_type (state_type s, symbol_type& sym);
      /// Assignment, needed by push_back.
      stack_symbol_type& operator= (const stack_symbol_type& that);
    };

    /// Stack type.
    typedef stack<stack_symbol_type> stack_type;

    /// The stack.
    stack_type yystack_;

    /// Push a new state on the stack.
    /// \param m    a debug message to display
    ///             if null, no trace is output.
    /// \param s    the symbol
    /// \warning the contents of \a s.value is stolen.
    void yypush_ (const char* m, stack_symbol_type& s);

    /// Push a new look ahead token on the state on the stack.
    /// \param m    a debug message to display
    ///             if null, no trace is output.
    /// \param s    the state
    /// \param sym  the symbol (for its value and location).
    /// \warning the contents of \a s.value is stolen.
    void yypush_ (const char* m, state_type s, symbol_type& sym);

    /// Pop \a n symbols the three stacks.
    void yypop_ (unsigned int n = 1);

    /// Constants.
    enum
    {
      yyeof_ = 0,
      yylast_ = 230,     ///< Last index in yytable_.
      yynnts_ = 14,  ///< Number of nonterminal symbols.
      yyfinal_ = 3, ///< Termination state number.
      yyterror_ = 1,
      yyerrcode_ = 256,
      yyntokens_ = 34  ///< Number of tokens.
    };


    // User arguments.
    lexcontext &ctx;
  };

  // Symbol number corresponding to token number t.
  inline
  conj_parser::token_number_type
  conj_parser::yytranslate_ (token_type t)
  {
    static
    const token_number_type
    translate_table[] =
    {
     0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    33,     2,     2,     2,     2,    25,     2,
      26,    29,    24,    22,    18,    23,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    20,    30,
       2,    21,     2,    19,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    27,     2,    32,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    31,     2,    28,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17
    };
    const unsigned int user_token_number_max_ = 272;
    const token_number_type undef_token_ = 2;

    if (static_cast<int>(t) <= yyeof_)
      return yyeof_;
    else if (static_cast<unsigned int> (t) <= user_token_number_max_)
      return translate_table[t];
    else
      return undef_token_;
  }

  inline
  conj_parser::syntax_error::syntax_error (const location_type& l, const std::string& m)
    : std::runtime_error (m)
    , location (l)
  {}

  // basic_symbol.
  template <typename Base>
  inline
  conj_parser::basic_symbol<Base>::basic_symbol ()
    : value ()
  {}

  template <typename Base>
  inline
  conj_parser::basic_symbol<Base>::basic_symbol (const basic_symbol& other)
    : Base (other)
    , value ()
    , location (other.location)
  {
      switch (other.type_get ())
    {
      case 41: // stmt
      case 42: // com_stmt
      case 43: // var_defs
      case 44: // var_def1
      case 45: // exprs
      case 46: // c_expr1
      case 47: // expr
        value.copy< expression > (other.value);
        break;

      case 8: // NUMCONST
        value.copy< long > (other.value);
        break;

      case 7: // IDENTIFIER
      case 9: // STRINGCONST
        value.copy< std::string > (other.value);
        break;

      default:
        break;
    }

  }


  template <typename Base>
  inline
  conj_parser::basic_symbol<Base>::basic_symbol (typename Base::kind_type t, const semantic_type& v, const location_type& l)
    : Base (t)
    , value ()
    , location (l)
  {
    (void) v;
      switch (this->type_get ())
    {
      case 41: // stmt
      case 42: // com_stmt
      case 43: // var_defs
      case 44: // var_def1
      case 45: // exprs
      case 46: // c_expr1
      case 47: // expr
        value.copy< expression > (v);
        break;

      case 8: // NUMCONST
        value.copy< long > (v);
        break;

      case 7: // IDENTIFIER
      case 9: // STRINGCONST
        value.copy< std::string > (v);
        break;

      default:
        break;
    }
}


  // Implementation of basic_symbol constructor for each type.

  template <typename Base>
  conj_parser::basic_symbol<Base>::basic_symbol (typename Base::kind_type t, const location_type& l)
    : Base (t)
    , value ()
    , location (l)
  {}

  template <typename Base>
  conj_parser::basic_symbol<Base>::basic_symbol (typename Base::kind_type t, const expression v, const location_type& l)
    : Base (t)
    , value (v)
    , location (l)
  {}

  template <typename Base>
  conj_parser::basic_symbol<Base>::basic_symbol (typename Base::kind_type t, const long v, const location_type& l)
    : Base (t)
    , value (v)
    , location (l)
  {}

  template <typename Base>
  conj_parser::basic_symbol<Base>::basic_symbol (typename Base::kind_type t, const std::string v, const location_type& l)
    : Base (t)
    , value (v)
    , location (l)
  {}


  template <typename Base>
  inline
  conj_parser::basic_symbol<Base>::~basic_symbol ()
  {
    clear ();
  }

  template <typename Base>
  inline
  void
  conj_parser::basic_symbol<Base>::clear ()
  {
    // User destructor.
    symbol_number_type yytype = this->type_get ();
    basic_symbol<Base>& yysym = *this;
    (void) yysym;
    switch (yytype)
    {
   default:
      break;
    }

    // Type destructor.
    switch (yytype)
    {
      case 41: // stmt
      case 42: // com_stmt
      case 43: // var_defs
      case 44: // var_def1
      case 45: // exprs
      case 46: // c_expr1
      case 47: // expr
        value.template destroy< expression > ();
        break;

      case 8: // NUMCONST
        value.template destroy< long > ();
        break;

      case 7: // IDENTIFIER
      case 9: // STRINGCONST
        value.template destroy< std::string > ();
        break;

      default:
        break;
    }

    Base::clear ();
  }

  template <typename Base>
  inline
  bool
  conj_parser::basic_symbol<Base>::empty () const
  {
    return Base::type_get () == empty_symbol;
  }

  template <typename Base>
  inline
  void
  conj_parser::basic_symbol<Base>::move (basic_symbol& s)
  {
    super_type::move(s);
      switch (this->type_get ())
    {
      case 41: // stmt
      case 42: // com_stmt
      case 43: // var_defs
      case 44: // var_def1
      case 45: // exprs
      case 46: // c_expr1
      case 47: // expr
        value.move< expression > (s.value);
        break;

      case 8: // NUMCONST
        value.move< long > (s.value);
        break;

      case 7: // IDENTIFIER
      case 9: // STRINGCONST
        value.move< std::string > (s.value);
        break;

      default:
        break;
    }

    location = s.location;
  }

  // by_type.
  inline
  conj_parser::by_type::by_type ()
    : type (empty_symbol)
  {}

  inline
  conj_parser::by_type::by_type (const by_type& other)
    : type (other.type)
  {}

  inline
  conj_parser::by_type::by_type (token_type t)
    : type (yytranslate_ (t))
  {}

  inline
  void
  conj_parser::by_type::clear ()
  {
    type = empty_symbol;
  }

  inline
  void
  conj_parser::by_type::move (by_type& that)
  {
    type = that.type;
    that.clear ();
  }

  inline
  int
  conj_parser::by_type::type_get () const
  {
    return type;
  }

  inline
  conj_parser::token_type
  conj_parser::by_type::token () const
  {
    // YYTOKNUM[NUM] -- (External) token number corresponding to the
    // (internal) symbol number NUM (which must be that of a token).  */
    static
    const unsigned short int
    yytoken_number_[] =
    {
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,    44,    63,
      58,    61,    43,    45,    42,    38,    40,    91,   125,    41,
      59,   123,    93,    33
    };
    return static_cast<token_type> (yytoken_number_[type]);
  }
  // Implementation of make_symbol for each symbol type.
  conj_parser::symbol_type
  conj_parser::make_END (const location_type& l)
  {
    return symbol_type (token::END, l);
  }

  conj_parser::symbol_type
  conj_parser::make_RETURN (const location_type& l)
  {
    return symbol_type (token::RETURN, l);
  }

  conj_parser::symbol_type
  conj_parser::make_WHILE (const location_type& l)
  {
    return symbol_type (token::WHILE, l);
  }

  conj_parser::symbol_type
  conj_parser::make_IF (const location_type& l)
  {
    return symbol_type (token::IF, l);
  }

  conj_parser::symbol_type
  conj_parser::make_VAR (const location_type& l)
  {
    return symbol_type (token::VAR, l);
  }

  conj_parser::symbol_type
  conj_parser::make_IDENTIFIER (const std::string& v, const location_type& l)
  {
    return symbol_type (token::IDENTIFIER, v, l);
  }

  conj_parser::symbol_type
  conj_parser::make_NUMCONST (const long& v, const location_type& l)
  {
    return symbol_type (token::NUMCONST, v, l);
  }

  conj_parser::symbol_type
  conj_parser::make_STRINGCONST (const std::string& v, const location_type& l)
  {
    return symbol_type (token::STRINGCONST, v, l);
  }

  conj_parser::symbol_type
  conj_parser::make_OR (const location_type& l)
  {
    return symbol_type (token::OR, l);
  }

  conj_parser::symbol_type
  conj_parser::make_AND (const location_type& l)
  {
    return symbol_type (token::AND, l);
  }

  conj_parser::symbol_type
  conj_parser::make_EQ (const location_type& l)
  {
    return symbol_type (token::EQ, l);
  }

  conj_parser::symbol_type
  conj_parser::make_NE (const location_type& l)
  {
    return symbol_type (token::NE, l);
  }

  conj_parser::symbol_type
  conj_parser::make_PP (const location_type& l)
  {
    return symbol_type (token::PP, l);
  }

  conj_parser::symbol_type
  conj_parser::make_MM (const location_type& l)
  {
    return symbol_type (token::MM, l);
  }

  conj_parser::symbol_type
  conj_parser::make_PL_EQ (const location_type& l)
  {
    return symbol_type (token::PL_EQ, l);
  }

  conj_parser::symbol_type
  conj_parser::make_MI_EQ (const location_type& l)
  {
    return symbol_type (token::MI_EQ, l);
  }



} // yy
#line 1596 "conj.c" // lalr1.cc:408





// User implementation prologue.

#line 1604 "conj.c" // lalr1.cc:412
// Unqualified %code blocks.
#line 93 "GrammarConfig.y" // lalr1.cc:413

	struct lexcontext
	{
		std::vector<std::map<std::string, identifier> > scopes;
		std::vector<function> func_list;
		unsigned tempcounter = 0;
		function fun;
		public:
			const identifier &define(const std::string &name, identifier &&f)
			{
				auto r = scopes.back().emplace(name, std::move(f));
				return r.first->second;
			}
			expression def(const std::string &name)		{ return define(name, identifier{id_type::variable, fun.num_vars++, name}); }
			expression defun(const std::string &name)	{ return define(name, identifier{id_type::function, func_list.size(), name}); }
			expression defparam(const std::string &name){ return define(name, identifier{id_type::parameter, fun.num_params++, name}); }
			expression temp()							{ return def("$I" + std::to_string(tempcounter++)); }
			expression use(const std::string &name)
			{
				for (auto j = scopes.crbegin(); j != scopes.crend(); ++j)
					if(auto i = j->find(name); i != j->end())
						return i->second;
			}
			void add_function(std::string &&name, expression &&code)
			{
				fun.code = e_comma(std::move(code), e_ret(0l)); //implicit return 0 for functions that don't include a return statement
				fun.name = std::move(name);
				func_list.push_back(std::move(fun));
				fun = {};
			}
			void operator ++() { scopes.emplace_back(); }	//entering a new scope
			void operator --() { scopes.pop_back(); }		//exiting scope
	};

	#define M(x) std::move(x)
	#define C(x) expression(x)

#line 1644 "conj.c" // lalr1.cc:413


#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> // FIXME: INFRINGES ON USER NAME SPACE.
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

#define YYRHSLOC(Rhs, K) ((Rhs)[K].location)
/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

# ifndef YYLLOC_DEFAULT
#  define YYLLOC_DEFAULT(Current, Rhs, N)                               \
    do                                                                  \
      if (N)                                                            \
        {                                                               \
          (Current).begin  = YYRHSLOC (Rhs, 1).begin;                   \
          (Current).end    = YYRHSLOC (Rhs, N).end;                     \
        }                                                               \
      else                                                              \
        {                                                               \
          (Current).begin = (Current).end = YYRHSLOC (Rhs, 0).end;      \
        }                                                               \
    while (/*CONSTCOND*/ false)
# endif


// Suppress unused-variable warnings by "using" E.
#define YYUSE(E) ((void) (E))

// Enable debugging if requested.
#if YYDEBUG

// A pseudo ostream that takes yydebug_ into account.
# define YYCDEBUG if (yydebug_) (*yycdebug_)

# define YY_SYMBOL_PRINT(Title, Symbol)         \
  do {                                          \
    if (yydebug_)                               \
    {                                           \
      *yycdebug_ << Title << ' ';               \
      yy_print_ (*yycdebug_, Symbol);           \
      *yycdebug_ << std::endl;                  \
    }                                           \
  } while (false)

# define YY_REDUCE_PRINT(Rule)          \
  do {                                  \
    if (yydebug_)                       \
      yy_reduce_print_ (Rule);          \
  } while (false)

# define YY_STACK_PRINT()               \
  do {                                  \
    if (yydebug_)                       \
      yystack_print_ ();                \
  } while (false)

#else // !YYDEBUG

# define YYCDEBUG if (false) std::cerr
# define YY_SYMBOL_PRINT(Title, Symbol)  YYUSE(Symbol)
# define YY_REDUCE_PRINT(Rule)           static_cast<void>(0)
# define YY_STACK_PRINT()                static_cast<void>(0)

#endif // !YYDEBUG

#define yyerrok         (yyerrstatus_ = 0)
#define yyclearin       (yyla.clear ())

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYRECOVERING()  (!!yyerrstatus_)


namespace yy {
#line 1730 "conj.c" // lalr1.cc:479

  /* Return YYSTR after stripping away unnecessary quotes and
     backslashes, so that it's suitable for yyerror.  The heuristic is
     that double-quoting is unnecessary unless the string contains an
     apostrophe, a comma, or backslash (other than backslash-backslash).
     YYSTR is taken from yytname.  */
  std::string
  conj_parser::yytnamerr_ (const char *yystr)
  {
    if (*yystr == '"')
      {
        std::string yyr = "";
        char const *yyp = yystr;

        for (;;)
          switch (*++yyp)
            {
            case '\'':
            case ',':
              goto do_not_strip_quotes;

            case '\\':
              if (*++yyp != '\\')
                goto do_not_strip_quotes;
              // Fall through.
            default:
              yyr += *yyp;
              break;

            case '"':
              return yyr;
            }
      do_not_strip_quotes: ;
      }

    return yystr;
  }


  /// Build a parser object.
  conj_parser::conj_parser (lexcontext &ctx_yyarg)
    :
#if YYDEBUG
      yydebug_ (false),
      yycdebug_ (&std::cerr),
#endif
      ctx (ctx_yyarg)
  {}

  conj_parser::~conj_parser ()
  {}


  /*---------------.
  | Symbol types.  |
  `---------------*/



  // by_state.
  inline
  conj_parser::by_state::by_state ()
    : state (empty_state)
  {}

  inline
  conj_parser::by_state::by_state (const by_state& other)
    : state (other.state)
  {}

  inline
  void
  conj_parser::by_state::clear ()
  {
    state = empty_state;
  }

  inline
  void
  conj_parser::by_state::move (by_state& that)
  {
    state = that.state;
    that.clear ();
  }

  inline
  conj_parser::by_state::by_state (state_type s)
    : state (s)
  {}

  inline
  conj_parser::symbol_number_type
  conj_parser::by_state::type_get () const
  {
    if (state == empty_state)
      return empty_symbol;
    else
      return yystos_[state];
  }

  inline
  conj_parser::stack_symbol_type::stack_symbol_type ()
  {}


  inline
  conj_parser::stack_symbol_type::stack_symbol_type (state_type s, symbol_type& that)
    : super_type (s, that.location)
  {
      switch (that.type_get ())
    {
      case 41: // stmt
      case 42: // com_stmt
      case 43: // var_defs
      case 44: // var_def1
      case 45: // exprs
      case 46: // c_expr1
      case 47: // expr
        value.move< expression > (that.value);
        break;

      case 8: // NUMCONST
        value.move< long > (that.value);
        break;

      case 7: // IDENTIFIER
      case 9: // STRINGCONST
        value.move< std::string > (that.value);
        break;

      default:
        break;
    }

    // that is emptied.
    that.type = empty_symbol;
  }

  inline
  conj_parser::stack_symbol_type&
  conj_parser::stack_symbol_type::operator= (const stack_symbol_type& that)
  {
    state = that.state;
      switch (that.type_get ())
    {
      case 41: // stmt
      case 42: // com_stmt
      case 43: // var_defs
      case 44: // var_def1
      case 45: // exprs
      case 46: // c_expr1
      case 47: // expr
        value.copy< expression > (that.value);
        break;

      case 8: // NUMCONST
        value.copy< long > (that.value);
        break;

      case 7: // IDENTIFIER
      case 9: // STRINGCONST
        value.copy< std::string > (that.value);
        break;

      default:
        break;
    }

    location = that.location;
    return *this;
  }


  template <typename Base>
  inline
  void
  conj_parser::yy_destroy_ (const char* yymsg, basic_symbol<Base>& yysym) const
  {
    if (yymsg)
      YY_SYMBOL_PRINT (yymsg, yysym);
  }

#if YYDEBUG
  template <typename Base>
  void
  conj_parser::yy_print_ (std::ostream& yyo,
                                     const basic_symbol<Base>& yysym) const
  {
    std::ostream& yyoutput = yyo;
    YYUSE (yyoutput);
    symbol_number_type yytype = yysym.type_get ();
    // Avoid a (spurious) G++ 4.8 warning about "array subscript is
    // below array bounds".
    if (yysym.empty ())
      std::abort ();
    yyo << (yytype < yyntokens_ ? "token" : "nterm")
        << ' ' << yytname_[yytype] << " ("
        << yysym.location << ": ";
    YYUSE (yytype);
    yyo << ')';
  }
#endif

  inline
  void
  conj_parser::yypush_ (const char* m, state_type s, symbol_type& sym)
  {
    stack_symbol_type t (s, sym);
    yypush_ (m, t);
  }

  inline
  void
  conj_parser::yypush_ (const char* m, stack_symbol_type& s)
  {
    if (m)
      YY_SYMBOL_PRINT (m, s);
    yystack_.push (s);
  }

  inline
  void
  conj_parser::yypop_ (unsigned int n)
  {
    yystack_.pop (n);
  }

#if YYDEBUG
  std::ostream&
  conj_parser::debug_stream () const
  {
    return *yycdebug_;
  }

  void
  conj_parser::set_debug_stream (std::ostream& o)
  {
    yycdebug_ = &o;
  }


  conj_parser::debug_level_type
  conj_parser::debug_level () const
  {
    return yydebug_;
  }

  void
  conj_parser::set_debug_level (debug_level_type l)
  {
    yydebug_ = l;
  }
#endif // YYDEBUG

  inline conj_parser::state_type
  conj_parser::yy_lr_goto_state_ (state_type yystate, int yysym)
  {
    int yyr = yypgoto_[yysym - yyntokens_] + yystate;
    if (0 <= yyr && yyr <= yylast_ && yycheck_[yyr] == yystate)
      return yytable_[yyr];
    else
      return yydefgoto_[yysym - yyntokens_];
  }

  inline bool
  conj_parser::yy_pact_value_is_default_ (int yyvalue)
  {
    return yyvalue == yypact_ninf_;
  }

  inline bool
  conj_parser::yy_table_value_is_error_ (int yyvalue)
  {
    return yyvalue == yytable_ninf_;
  }

  int
  conj_parser::parse ()
  {
    // State.
    int yyn;
    /// Length of the RHS of the rule being reduced.
    int yylen = 0;

    // Error handling.
    int yynerrs_ = 0;
    int yyerrstatus_ = 0;

    /// The lookahead symbol.
    symbol_type yyla;

    /// The locations where the error started and ended.
    stack_symbol_type yyerror_range[3];

    /// The return value of parse ().
    int yyresult;

    // FIXME: This shoud be completely indented.  It is not yet to
    // avoid gratuitous conflicts when merging into the master branch.
    try
      {
    YYCDEBUG << "Starting parse" << std::endl;


    /* Initialize the stack.  The initial state will be set in
       yynewstate, since the latter expects the semantical and the
       location values to have been already stored, initialize these
       stacks with a primary value.  */
    yystack_.clear ();
    yypush_ (YY_NULLPTR, 0, yyla);

    // A new symbol was pushed on the stack.
  yynewstate:
    YYCDEBUG << "Entering state " << yystack_[0].state << std::endl;

    // Accept?
    if (yystack_[0].state == yyfinal_)
      goto yyacceptlab;

    goto yybackup;

    // Backup.
  yybackup:

    // Try to take a decision without lookahead.
    yyn = yypact_[yystack_[0].state];
    if (yy_pact_value_is_default_ (yyn))
      goto yydefault;

    // Read a lookahead token.
    if (yyla.empty ())
      {
        YYCDEBUG << "Reading a token: ";
        try
          {
            symbol_type yylookahead (yylex (ctx));
            yyla.move (yylookahead);
          }
        catch (const syntax_error& yyexc)
          {
            error (yyexc);
            goto yyerrlab1;
          }
      }
    YY_SYMBOL_PRINT ("Next token is", yyla);

    /* If the proper action on seeing token YYLA.TYPE is to reduce or
       to detect an error, take that action.  */
    yyn += yyla.type_get ();
    if (yyn < 0 || yylast_ < yyn || yycheck_[yyn] != yyla.type_get ())
      goto yydefault;

    // Reduce or error.
    yyn = yytable_[yyn];
    if (yyn <= 0)
      {
        if (yy_table_value_is_error_ (yyn))
          goto yyerrlab;
        yyn = -yyn;
        goto yyreduce;
      }

    // Count tokens shifted since error; after three, turn off error status.
    if (yyerrstatus_)
      --yyerrstatus_;

    // Shift the lookahead token.
    yypush_ ("Shifting", yyn, yyla);
    goto yynewstate;

  /*-----------------------------------------------------------.
  | yydefault -- do the default action for the current state.  |
  `-----------------------------------------------------------*/
  yydefault:
    yyn = yydefact_[yystack_[0].state];
    if (yyn == 0)
      goto yyerrlab;
    goto yyreduce;

  /*-----------------------------.
  | yyreduce -- Do a reduction.  |
  `-----------------------------*/
  yyreduce:
    yylen = yyr2_[yyn];
    {
      stack_symbol_type yylhs;
      yylhs.state = yy_lr_goto_state_(yystack_[yylen].state, yyr1_[yyn]);
      /* Variants are always initialized to an empty instance of the
         correct type. The default '$$ = $1' action is NOT applied
         when using variants.  */
        switch (yyr1_[yyn])
    {
      case 41: // stmt
      case 42: // com_stmt
      case 43: // var_defs
      case 44: // var_def1
      case 45: // exprs
      case 46: // c_expr1
      case 47: // expr
        yylhs.value.build< expression > ();
        break;

      case 8: // NUMCONST
        yylhs.value.build< long > ();
        break;

      case 7: // IDENTIFIER
      case 9: // STRINGCONST
        yylhs.value.build< std::string > ();
        break;

      default:
        break;
    }


      // Compute the default @$.
      {
        slice<stack_symbol_type, stack_type> slice (yystack_, yylen);
        YYLLOC_DEFAULT (yylhs.location, slice, yylen);
      }

      // Perform the reduction.
      YY_REDUCE_PRINT (yyn);
      try
        {
          switch (yyn)
            {
  case 2:
#line 148 "GrammarConfig.y" // lalr1.cc:859
    { ++ctx; }
#line 2162 "conj.c" // lalr1.cc:859
    break;

  case 3:
#line 148 "GrammarConfig.y" // lalr1.cc:859
    { --ctx; }
#line 2168 "conj.c" // lalr1.cc:859
    break;

  case 4:
#line 149 "GrammarConfig.y" // lalr1.cc:859
    { ctx.defun(yystack_[0].value.as< std::string > ()); ++ctx; }
#line 2174 "conj.c" // lalr1.cc:859
    break;

  case 5:
#line 149 "GrammarConfig.y" // lalr1.cc:859
    { ctx.add_functions(M(yystack_[4].value.as< std::string > ()), M(yystack_[0].value.as< expression > ())); --ctx; }
#line 2180 "conj.c" // lalr1.cc:859
    break;

  case 9:
#line 153 "GrammarConfig.y" // lalr1.cc:859
    { ctx.defparam(yystack_[0].value.as< std::string > ()); }
#line 2186 "conj.c" // lalr1.cc:859
    break;

  case 10:
#line 154 "GrammarConfig.y" // lalr1.cc:859
    { ctx.defparam(yystack_[0].value.as< std::string > ()); }
#line 2192 "conj.c" // lalr1.cc:859
    break;

  case 11:
#line 155 "GrammarConfig.y" // lalr1.cc:859
    { yylhs.value.as< expression > () = M(yystack_[1].value.as< expression > ()); --ctx; }
#line 2198 "conj.c" // lalr1.cc:859
    break;

  case 12:
#line 156 "GrammarConfig.y" // lalr1.cc:859
    { yylhs.value.as< expression > () = e_cand(M(yystack_[2].value.as< expression > ()), M(yystack_[0].value.as< expression > ())); }
#line 2204 "conj.c" // lalr1.cc:859
    break;

  case 13:
#line 157 "GrammarConfig.y" // lalr1.cc:859
    { yylhs.value.as< expression > () = e_loop(M(yystack_[2].value.as< expression > ()), M(yystack_[0].value.as< expression > ())); }
#line 2210 "conj.c" // lalr1.cc:859
    break;

  case 14:
#line 158 "GrammarConfig.y" // lalr1.cc:859
    { yylhs.value.as< expression > () = e_ret(M(yystack_[1].value.as< expression > ())); }
#line 2216 "conj.c" // lalr1.cc:859
    break;

  case 15:
#line 159 "GrammarConfig.y" // lalr1.cc:859
    { yylhs.value.as< expression > () = M(yystack_[1].value.as< expression > ()); }
#line 2222 "conj.c" // lalr1.cc:859
    break;

  case 16:
#line 160 "GrammarConfig.y" // lalr1.cc:859
    { }
#line 2228 "conj.c" // lalr1.cc:859
    break;

  case 17:
#line 161 "GrammarConfig.y" // lalr1.cc:859
    { yylhs.value.as< expression > () = e_comma(); ++ctx; }
#line 2234 "conj.c" // lalr1.cc:859
    break;

  case 18:
#line 162 "GrammarConfig.y" // lalr1.cc:859
    { yylhs.value.as< expression > () = M(yystack_[1].value.as< expression > ()); yylhs.value.as< expression > ().params.push_back(M(yystack_[0].value.as< expression > ())); }
#line 2240 "conj.c" // lalr1.cc:859
    break;

  case 19:
#line 163 "GrammarConfig.y" // lalr1.cc:859
    { yylhs.value.as< expression > () = e_comma(M(yystack_[0].value.as< expression > ())); }
#line 2246 "conj.c" // lalr1.cc:859
    break;

  case 20:
#line 164 "GrammarConfig.y" // lalr1.cc:859
    { yylhs.value.as< expression > () = M(yystack_[2].value.as< expression > ()); yylhs.value.as< expression > ().params.push_backs(M(yystack_[0].value.as< expression > ())); }
#line 2252 "conj.c" // lalr1.cc:859
    break;

  case 21:
#line 165 "GrammarConfig.y" // lalr1.cc:859
    { yylhs.value.as< expression > () = ctx.def(yystack_[2].value.as< std::string > ()) %= M(yystack_[0].value.as< expression > ()); }
#line 2258 "conj.c" // lalr1.cc:859
    break;

  case 22:
#line 166 "GrammarConfig.y" // lalr1.cc:859
    { yylhs.value.as< expression > () = ctx.def(yystack_[0].value.as< std::string > ()) %= 0l; }
#line 2264 "conj.c" // lalr1.cc:859
    break;

  case 23:
#line 167 "GrammarConfig.y" // lalr1.cc:859
    { yylhs.value.as< expression > () = M(yystack_[0].value.as< expression > ()); }
#line 2270 "conj.c" // lalr1.cc:859
    break;

  case 24:
#line 168 "GrammarConfig.y" // lalr1.cc:859
    { yylhs.value.as< expression > () = M(yystack_[0].value.as< expression > ()); }
#line 2276 "conj.c" // lalr1.cc:859
    break;

  case 25:
#line 169 "GrammarConfig.y" // lalr1.cc:859
    { yylhs.value.as< expression > () = e_comma(M(yystack_[2].value.as< expression > ())); yylhs.value.as< expression > ().params.splice(yylhs.value.as< expression > ().params.end(), M(yystack_[0].value.as< expression > ().params)); }
#line 2282 "conj.c" // lalr1.cc:859
    break;

  case 26:
#line 170 "GrammarConfig.y" // lalr1.cc:859
    { yylhs.value.as< expression > () = e_comma(M(yystack_[0].value.as< expression > ())); }
#line 2288 "conj.c" // lalr1.cc:859
    break;

  case 27:
#line 171 "GrammarConfig.y" // lalr1.cc:859
    { yylhs.value.as< expression > () = M(yystack_[2].value.as< expression > ()); yylhs.value.as< expression > ().params.push_back(M(yystack_[0].value.as< expression > ())); }
#line 2294 "conj.c" // lalr1.cc:859
    break;

  case 28:
#line 172 "GrammarConfig.y" // lalr1.cc:859
    { yylhs.value.as< expression > () = yystack_[0].value.as< long > (); }
#line 2300 "conj.c" // lalr1.cc:859
    break;

  case 29:
#line 173 "GrammarConfig.y" // lalr1.cc:859
    { yylhs.value.as< expression > () = M(yystack_[0].value.as< std::string > ()); }
#line 2306 "conj.c" // lalr1.cc:859
    break;

  case 30:
#line 174 "GrammarConfig.y" // lalr1.cc:859
    { yylhs.value.as< expression > () = ctx.use(yystack_[0].value.as< std::string > ()); }
#line 2312 "conj.c" // lalr1.cc:859
    break;

  case 31:
#line 175 "GrammarConfig.y" // lalr1.cc:859
    { yylhs.value.as< expression > () = M(yystack_[1].value.as< expression > ()); }
#line 2318 "conj.c" // lalr1.cc:859
    break;

  case 32:
#line 176 "GrammarConfig.y" // lalr1.cc:859
    { yylhs.value.as< expression > () = e_deref(e_add(Myystack_[3].value.as< expression > ()), M(yystack_[1].value.as< expression > ()))); }
#line 2324 "conj.c" // lalr1.cc:859
    break;

  case 33:
#line 177 "GrammarConfig.y" // lalr1.cc:859
    { yylhs.value.as< expression > () = e_fcall(M(yystack_[2].value.as< expression > ())); }
#line 2330 "conj.c" // lalr1.cc:859
    break;

  case 34:
#line 178 "GrammarConfig.y" // lalr1.cc:859
    { yylhs.value.as< expression > () = e_fcall(M(yystack_[3].value.as< expression > ())); yylhs.value.as< expression > ().params.splice(yylhs.value.as< expression > ().params.end(), M(yystack_[1].value.as< expression > ().params)); }
#line 2336 "conj.c" // lalr1.cc:859
    break;

  case 35:
#line 179 "GrammarConfig.y" // lalr1.cc:859
    { yylhs.value.as< expression > () = M(yystack_[2].value.as< expression > ()) %= M(yystack_[0].value.as< expression > ()); }
#line 2342 "conj.c" // lalr1.cc:859
    break;

  case 36:
#line 180 "GrammarConfig.y" // lalr1.cc:859
    { yylhs.value.as< expression > () = e_add(M(yystack_[2].value.as< expression > ()), M(yystack_[0].value.as< expression > ())); }
#line 2348 "conj.c" // lalr1.cc:859
    break;

  case 37:
#line 181 "GrammarConfig.y" // lalr1.cc:859
    { yylhs.value.as< expression > () = e_add(M(yystack_[2].value.as< expression > ()), e_neg(M(yystack_[0].value.as< expression > ()))); }
#line 2354 "conj.c" // lalr1.cc:859
    break;

  case 38:
#line 182 "GrammarConfig.y" // lalr1.cc:859
    { if (!yystack_[0].value.as< expression > ().is_pure()) { yylhs.value.as< expression > () = ctx.temp() %= e_addrof(M(yystack_[2].value.as< expression > ())); yystack_[2].value.as< expression > () = e_deref(yylhs.value.as< expression > ().params.back()); }
										  yylhs.value.as< expression > () = e_comma(M(yylhs.value.as< expression > ()), M(yystack_[2].value.as< expression > ()) %= e_add(C(yystack_[2].value.as< expression > ()), M(yystack_[0].value.as< expression > ()))); }
#line 2361 "conj.c" // lalr1.cc:859
    break;

  case 39:
#line 185 "GrammarConfig.y" // lalr1.cc:859
    { if (!yystack_[0].value.as< expression > ().is_pure()) { yylhs.value.as< expression > () = ctx.temp() %= e_addrof(M(yystack_[2].value.as< expression > ())); yystack_[2].value.as< expression > () = e_deref(yylhs.value.as< expression > ().params.back()); }
										  yylhs.value.as< expression > () = e_comma(M(yylhs.value.as< expression > ()), M(yystack_[2].value.as< expression > ()) %= e_add(C(yystack_[2].value.as< expression > ()), e_neg(M(yystack_[0].value.as< expression > ())))); }
#line 2368 "conj.c" // lalr1.cc:859
    break;

  case 40:
#line 188 "GrammarConfig.y" // lalr1.cc:859
    { if (!yystack_[0].value.as< expression > ().is_pure()) { yylhs.value.as< expression > () = ctx.temp() %= e_addrof(M(yystack_[0].value.as< expression > ())); yystack_[0].value.as< expression > () = e_deref(yylhs.value.as< expression > ().params.back()); }
										  yylhs.value.as< expression > () = e_comma(M(yylhs.value.as< expression > ()), M(yystack_[0].value.as< expression > ()) %= e_add(C(yystack_[0].value.as< expression > ()), 1l)); }
#line 2375 "conj.c" // lalr1.cc:859
    break;

  case 41:
#line 191 "GrammarConfig.y" // lalr1.cc:859
    { if (!yystack_[0].value.as< expression > ().is_pure()) { yylhs.value.as< expression > () = ctx.temp() %= e_addrof(M(yystack_[0].value.as< expression > ())); yystack_[0].value.as< expression > () = e_deref(yylhs.value.as< expression > ().params.back()); }
										  yylhs.value.as< expression > () = e_comma(M(yylhs.value.as< expression > ()), M(yystack_[0].value.as< expression > ()) %= e_add(C(yystack_[0].value.as< expression > ()), -1l)); }
#line 2382 "conj.c" // lalr1.cc:859
    break;

  case 42:
#line 194 "GrammarConfig.y" // lalr1.cc:859
    { if (!yystack_[1].value.as< expression > ().is_pure()) { yylhs.value.as< expression > () = ctx.temp() %= e_addrof(M(yystack_[1].value.as< expression > ())); yystack_[1].value.as< expression > () = e_deref(yylhs.value.as< expression > ().params.back()); }
										  auto i = ctx.temp(); yylhs.value.as< expression > () = e_comma(M(yylhs.value.as< expression > ()), C(i) %= C(yystack_[1].value.as< expression > ()), C(yystack_[1].value.as< expression > ()) %= e_add(C(yystack_[1].value.as< expression > ()), 1l), C(i)); }
#line 2389 "conj.c" // lalr1.cc:859
    break;

  case 43:
#line 197 "GrammarConfig.y" // lalr1.cc:859
    { if (!yystack_[1].value.as< expression > ().is_pure()) { yylhs.value.as< expression > () = ctx.temp() %= e_addrof(M(yystack_[1].value.as< expression > ())); yystack_[1].value.as< expression > () = e_deref(yylhs.value.as< expression > ().params.back()); }
										  auto i = ctx.temp(); yylhs.value.as< expression > () = e_comma(M(yylhs.value.as< expression > ()), C(i) %= C(yystack_[1].value.as< expression > ()), C(yystack_[1].value.as< expression > ()) %= e_add(C(yystack_[1].value.as< expression > ()), -1l), C(i)); }
#line 2396 "conj.c" // lalr1.cc:859
    break;

  case 45:
#line 201 "GrammarConfig.y" // lalr1.cc:859
    { yylhs.value.as< expression > () = e_cor(M(yystack_[2].value.as< expression > ()), M(yystack_[0].value.as< expression > ())); }
#line 2402 "conj.c" // lalr1.cc:859
    break;

  case 46:
#line 202 "GrammarConfig.y" // lalr1.cc:859
    { yylhs.value.as< expression > () = e_cand(M(yystack_[2].value.as< expression > ()), M(yystack_[0].value.as< expression > ())); }
#line 2408 "conj.c" // lalr1.cc:859
    break;

  case 47:
#line 203 "GrammarConfig.y" // lalr1.cc:859
    { yylhs.value.as< expression > () = e_eq(M(yystack_[2].value.as< expression > ()), M(yystack_[0].value.as< expression > ())); }
#line 2414 "conj.c" // lalr1.cc:859
    break;

  case 48:
#line 204 "GrammarConfig.y" // lalr1.cc:859
    { yylhs.value.as< expression > () = e_eq(e_eq(M(yystack_[2].value.as< expression > ()), M(yystack_[0].value.as< expression > ())), 0l); }
#line 2420 "conj.c" // lalr1.cc:859
    break;

  case 49:
#line 205 "GrammarConfig.y" // lalr1.cc:859
    { yylhs.value.as< expression > () = e_addrof(M(yystack_[0].value.as< expression > ())); }
#line 2426 "conj.c" // lalr1.cc:859
    break;

  case 50:
#line 206 "GrammarConfig.y" // lalr1.cc:859
    { yylhs.value.as< expression > () = e_deref(M(yystack_[0].value.as< expression > ())); }
#line 2432 "conj.c" // lalr1.cc:859
    break;

  case 51:
#line 207 "GrammarConfig.y" // lalr1.cc:859
    { yylhs.value.as< expression > () = e_neg(M(yystack_[0].value.as< expression > ())); }
#line 2438 "conj.c" // lalr1.cc:859
    break;

  case 52:
#line 208 "GrammarConfig.y" // lalr1.cc:859
    { yylhs.value.as< expression > () = e_eq(M(yystack_[0].value.as< expression > ()), 0l); }
#line 2444 "conj.c" // lalr1.cc:859
    break;


#line 2448 "conj.c" // lalr1.cc:859
            default:
              break;
            }
        }
      catch (const syntax_error& yyexc)
        {
          error (yyexc);
          YYERROR;
        }
      YY_SYMBOL_PRINT ("-> $$ =", yylhs);
      yypop_ (yylen);
      yylen = 0;
      YY_STACK_PRINT ();

      // Shift the result of the reduction.
      yypush_ (YY_NULLPTR, yylhs);
    }
    goto yynewstate;

  /*--------------------------------------.
  | yyerrlab -- here on detecting error.  |
  `--------------------------------------*/
  yyerrlab:
    // If not already recovering from an error, report this error.
    if (!yyerrstatus_)
      {
        ++yynerrs_;
        error (yyla.location, yysyntax_error_ (yystack_[0].state, yyla));
      }


    yyerror_range[1].location = yyla.location;
    if (yyerrstatus_ == 3)
      {
        /* If just tried and failed to reuse lookahead token after an
           error, discard it.  */

        // Return failure if at end of input.
        if (yyla.type_get () == yyeof_)
          YYABORT;
        else if (!yyla.empty ())
          {
            yy_destroy_ ("Error: discarding", yyla);
            yyla.clear ();
          }
      }

    // Else will try to reuse lookahead token after shifting the error token.
    goto yyerrlab1;


  /*---------------------------------------------------.
  | yyerrorlab -- error raised explicitly by YYERROR.  |
  `---------------------------------------------------*/
  yyerrorlab:

    /* Pacify compilers like GCC when the user code never invokes
       YYERROR and the label yyerrorlab therefore never appears in user
       code.  */
    if (false)
      goto yyerrorlab;
    yyerror_range[1].location = yystack_[yylen - 1].location;
    /* Do not reclaim the symbols of the rule whose action triggered
       this YYERROR.  */
    yypop_ (yylen);
    yylen = 0;
    goto yyerrlab1;

  /*-------------------------------------------------------------.
  | yyerrlab1 -- common code for both syntax error and YYERROR.  |
  `-------------------------------------------------------------*/
  yyerrlab1:
    yyerrstatus_ = 3;   // Each real token shifted decrements this.
    {
      stack_symbol_type error_token;
      for (;;)
        {
          yyn = yypact_[yystack_[0].state];
          if (!yy_pact_value_is_default_ (yyn))
            {
              yyn += yyterror_;
              if (0 <= yyn && yyn <= yylast_ && yycheck_[yyn] == yyterror_)
                {
                  yyn = yytable_[yyn];
                  if (0 < yyn)
                    break;
                }
            }

          // Pop the current state because it cannot handle the error token.
          if (yystack_.size () == 1)
            YYABORT;

          yyerror_range[1].location = yystack_[0].location;
          yy_destroy_ ("Error: popping", yystack_[0]);
          yypop_ ();
          YY_STACK_PRINT ();
        }

      yyerror_range[2].location = yyla.location;
      YYLLOC_DEFAULT (error_token.location, yyerror_range, 2);

      // Shift the error token.
      error_token.state = yyn;
      yypush_ ("Shifting", error_token);
    }
    goto yynewstate;

    // Accept.
  yyacceptlab:
    yyresult = 0;
    goto yyreturn;

    // Abort.
  yyabortlab:
    yyresult = 1;
    goto yyreturn;

  yyreturn:
    if (!yyla.empty ())
      yy_destroy_ ("Cleanup: discarding lookahead", yyla);

    /* Do not reclaim the symbols of the rule whose action triggered
       this YYABORT or YYACCEPT.  */
    yypop_ (yylen);
    while (1 < yystack_.size ())
      {
        yy_destroy_ ("Cleanup: popping", yystack_[0]);
        yypop_ ();
      }

    return yyresult;
  }
    catch (...)
      {
        YYCDEBUG << "Exception caught: cleaning lookahead and stack"
                 << std::endl;
        // Do not try to display the values of the reclaimed symbols,
        // as their printer might throw an exception.
        if (!yyla.empty ())
          yy_destroy_ (YY_NULLPTR, yyla);

        while (1 < yystack_.size ())
          {
            yy_destroy_ (YY_NULLPTR, yystack_[0]);
            yypop_ ();
          }
        throw;
      }
  }

  void
  conj_parser::error (const syntax_error& yyexc)
  {
    error (yyexc.location, yyexc.what());
  }

  // Generate an error message.
  std::string
  conj_parser::yysyntax_error_ (state_type yystate, const symbol_type& yyla) const
  {
    // Number of reported tokens (one for the "unexpected", one per
    // "expected").
    size_t yycount = 0;
    // Its maximum.
    enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
    // Arguments of yyformat.
    char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];

    /* There are many possibilities here to consider:
       - If this state is a consistent state with a default action, then
         the only way this function was invoked is if the default action
         is an error action.  In that case, don't check for expected
         tokens because there are none.
       - The only way there can be no lookahead present (in yyla) is
         if this state is a consistent state with a default action.
         Thus, detecting the absence of a lookahead is sufficient to
         determine that there is no unexpected or expected token to
         report.  In that case, just report a simple "syntax error".
       - Don't assume there isn't a lookahead just because this state is
         a consistent state with a default action.  There might have
         been a previous inconsistent state, consistent state with a
         non-default action, or user semantic action that manipulated
         yyla.  (However, yyla is currently not documented for users.)
       - Of course, the expected token list depends on states to have
         correct lookahead information, and it depends on the parser not
         to perform extra reductions after fetching a lookahead from the
         scanner and before detecting a syntax error.  Thus, state
         merging (from LALR or IELR) and default reductions corrupt the
         expected token list.  However, the list is correct for
         canonical LR with one exception: it will still contain any
         token that will not be accepted due to an error action in a
         later state.
    */
    if (!yyla.empty ())
      {
        int yytoken = yyla.type_get ();
        yyarg[yycount++] = yytname_[yytoken];
        int yyn = yypact_[yystate];
        if (!yy_pact_value_is_default_ (yyn))
          {
            /* Start YYX at -YYN if negative to avoid negative indexes in
               YYCHECK.  In other words, skip the first -YYN actions for
               this state because they are default actions.  */
            int yyxbegin = yyn < 0 ? -yyn : 0;
            // Stay within bounds of both yycheck and yytname.
            int yychecklim = yylast_ - yyn + 1;
            int yyxend = yychecklim < yyntokens_ ? yychecklim : yyntokens_;
            for (int yyx = yyxbegin; yyx < yyxend; ++yyx)
              if (yycheck_[yyx + yyn] == yyx && yyx != yyterror_
                  && !yy_table_value_is_error_ (yytable_[yyx + yyn]))
                {
                  if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                    {
                      yycount = 1;
                      break;
                    }
                  else
                    yyarg[yycount++] = yytname_[yyx];
                }
          }
      }

    char const* yyformat = YY_NULLPTR;
    switch (yycount)
      {
#define YYCASE_(N, S)                         \
        case N:                               \
          yyformat = S;                       \
        break
        YYCASE_(0, YY_("syntax error"));
        YYCASE_(1, YY_("syntax error, unexpected %s"));
        YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
        YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
        YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
        YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
#undef YYCASE_
      }

    std::string yyres;
    // Argument number.
    size_t yyi = 0;
    for (char const* yyp = yyformat; *yyp; ++yyp)
      if (yyp[0] == '%' && yyp[1] == 's' && yyi < yycount)
        {
          yyres += yytnamerr_ (yyarg[yyi++]);
          ++yyp;
        }
      else
        yyres += *yyp;
    return yyres;
  }


  const signed char conj_parser::yypact_ninf_ = -25;

  const signed char conj_parser::yytable_ninf_ = -1;

  const short int
  conj_parser::yypact_[] =
  {
     -25,     7,   -25,   -25,     5,   -25,     8,   -25,    -1,    -4,
     100,    13,   113,    -3,    10,    14,   -25,   -25,   -25,     2,
       2,     2,     2,     2,   113,   -25,   -25,     2,   -25,    69,
       4,    15,   157,   -25,    16,   113,   113,    29,   -25,    33,
      33,    33,    33,    33,    22,    33,   -25,   -25,    14,   -25,
       2,     2,     2,     2,   -25,   -25,     2,     2,     2,     2,
       2,     2,     2,   133,   113,   -25,    23,    32,     2,   -25,
     -25,    42,   138,   203,   203,   193,   193,    48,   193,   175,
     193,    33,    33,   -25,    -5,    35,   100,   100,   193,     2,
       2,   -25,   -25,   -25,   -25,   193,   193
  };

  const unsigned char
  conj_parser::yydefact_[] =
  {
       2,     0,     6,     1,     3,     4,     8,    10,     0,     7,
       0,     0,     0,     0,     0,     0,    30,    28,    29,     0,
       0,     0,     0,     0,     0,    16,    17,     0,     5,     0,
      23,     0,    24,     9,     0,     0,     0,    22,    19,    40,
      41,    51,    50,    49,     0,    52,    11,    18,     0,    15,
       0,     0,     0,     0,    42,    43,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    14,     0,     0,     0,    31,
      20,    45,    46,    47,    48,    38,    39,    25,    26,     0,
      35,    36,    37,    33,     0,     0,     0,     0,    21,     0,
       0,    34,    32,    13,    12,    27,    44
  };

  const signed char
  conj_parser::yypgoto_[] =
  {
     -25,   -25,   -25,   -25,   -25,   -25,   -25,   -24,   -25,   -25,
      31,    -6,    17,   -19
  };

  const signed char
  conj_parser::yydefgoto_[] =
  {
      -1,     1,     2,     4,     6,     8,     9,    28,    29,    30,
      38,    31,    77,    32
  };

  const unsigned char
  conj_parser::yytable_[] =
  {
      39,    40,    41,    42,    43,    47,    34,     3,    45,    16,
      17,    18,     5,    89,    11,     7,    19,    20,    44,    10,
      33,    37,    48,    35,    91,    21,    22,    23,    24,    66,
      67,    71,    72,    73,    74,    27,    36,    75,    76,    78,
      79,    80,    81,    82,    78,    49,    65,    54,    55,    88,
      68,    69,    86,    51,    52,    53,    54,    55,    85,    63,
      64,    87,    93,    94,    61,    62,    89,    92,    63,    64,
      95,    96,    12,    13,    14,    15,    16,    17,    18,    70,
      84,     0,     0,    19,    20,     0,     0,     0,     0,     0,
       0,     0,    21,    22,    23,    24,     0,    46,     0,    25,
      26,     0,    27,    12,    13,    14,    15,    16,    17,    18,
       0,     0,     0,     0,    19,    20,     0,     0,     0,    15,
      16,    17,    18,    21,    22,    23,    24,    19,    20,     0,
      25,    26,     0,    27,     0,     0,    21,    22,    23,    24,
      16,    17,    18,     0,     0,     0,    27,    19,    20,     0,
      52,    53,    54,    55,     0,     0,    21,    22,    23,    24,
      61,    62,    83,     0,    63,    64,    27,    50,    51,    52,
      53,    54,    55,    56,    57,    58,    59,     0,    60,    61,
      62,     0,     0,    63,    64,    50,    51,    52,    53,    54,
      55,    56,    57,     0,    59,    90,    60,    61,    62,     0,
       0,    63,    64,    50,    51,    52,    53,    54,    55,    56,
      57,     0,    59,     0,    60,    61,    62,    54,    55,    63,
      64,     0,     0,     0,     0,    61,    62,     0,     0,    63,
      64
  };

  const signed char
  conj_parser::yycheck_[] =
  {
      19,    20,    21,    22,    23,    29,    12,     0,    27,     7,
       8,     9,     7,    18,    18,     7,    14,    15,    24,    20,
       7,     7,    18,    26,    29,    23,    24,    25,    26,    35,
      36,    50,    51,    52,    53,    33,    26,    56,    57,    58,
      59,    60,    61,    62,    63,    30,    30,    14,    15,    68,
      21,    29,    29,    11,    12,    13,    14,    15,    64,    26,
      27,    29,    86,    87,    22,    23,    18,    32,    26,    27,
      89,    90,     3,     4,     5,     6,     7,     8,     9,    48,
      63,    -1,    -1,    14,    15,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    23,    24,    25,    26,    -1,    28,    -1,    30,
      31,    -1,    33,     3,     4,     5,     6,     7,     8,     9,
      -1,    -1,    -1,    -1,    14,    15,    -1,    -1,    -1,     6,
       7,     8,     9,    23,    24,    25,    26,    14,    15,    -1,
      30,    31,    -1,    33,    -1,    -1,    23,    24,    25,    26,
       7,     8,     9,    -1,    -1,    -1,    33,    14,    15,    -1,
      12,    13,    14,    15,    -1,    -1,    23,    24,    25,    26,
      22,    23,    29,    -1,    26,    27,    33,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    -1,    21,    22,
      23,    -1,    -1,    26,    27,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    19,    20,    21,    22,    23,    -1,
      -1,    26,    27,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    19,    -1,    21,    22,    23,    14,    15,    26,
      27,    -1,    -1,    -1,    -1,    22,    23,    -1,    -1,    26,
      27
  };

  const unsigned char
  conj_parser::yystos_[] =
  {
       0,    35,    36,     0,    37,     7,    38,     7,    39,    40,
      20,    18,     3,     4,     5,     6,     7,     8,     9,    14,
      15,    23,    24,    25,    26,    30,    31,    33,    41,    42,
      43,    45,    47,     7,    45,    26,    26,     7,    44,    47,
      47,    47,    47,    47,    45,    47,    28,    41,    18,    30,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      21,    22,    23,    26,    27,    30,    45,    45,    21,    29,
      44,    47,    47,    47,    47,    47,    47,    46,    47,    47,
      47,    47,    47,    29,    46,    45,    29,    29,    47,    18,
      20,    29,    32,    41,    41,    47,    47
  };

  const unsigned char
  conj_parser::yyr1_[] =
  {
       0,    34,    36,    35,    38,    37,    37,    39,    39,    40,
      40,    41,    41,    41,    41,    41,    41,    42,    42,    43,
      43,    44,    44,    45,    45,    45,    46,    46,    47,    47,
      47,    47,    47,    47,    47,    47,    47,    47,    47,    47,
      47,    47,    47,    47,    47,    47,    47,    47,    47,    47,
      47,    47,    47
  };

  const unsigned char
  conj_parser::yyr2_[] =
  {
       0,     2,     0,     2,     0,     6,     0,     1,     0,     3,
       1,     2,     5,     5,     3,     2,     1,     1,     2,     2,
       3,     3,     1,     1,     1,     3,     1,     3,     1,     1,
       1,     3,     4,     3,     4,     3,     3,     3,     3,     3,
       2,     2,     2,     2,     5,     3,     3,     3,     3,     2,
       2,     2,     2
  };



  // YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
  // First, the terminals, then, starting at \a yyntokens_, nonterminals.
  const char*
  const conj_parser::yytname_[] =
  {
  "END", "error", "$undefined", "\"return\"", "\"while\"", "\"if\"",
  "\"var\"", "IDENTIFIER", "NUMCONST", "STRINGCONST", "\"||\"", "\"&&\"",
  "\"==\"", "\"!=\"", "\"++\"", "\"--\"", "\"+=\"", "\"-=\"", "','", "'?'",
  "':'", "'='", "'+'", "'-'", "'*'", "'&'", "'('", "'['", "'}'", "')'",
  "';'", "'{'", "']'", "'!'", "$accept", "library", "$@1", "functions",
  "$@2", "paramdecls", "paramdecl", "stmt", "com_stmt", "var_defs",
  "var_def1", "exprs", "c_expr1", "expr", YY_NULLPTR
  };

#if YYDEBUG
  const unsigned char
  conj_parser::yyrline_[] =
  {
       0,   148,   148,   148,   149,   149,   150,   151,   152,   153,
     154,   155,   156,   157,   158,   159,   160,   161,   162,   163,
     164,   165,   166,   167,   168,   169,   170,   171,   172,   173,
     174,   175,   176,   177,   178,   179,   180,   181,   182,   185,
     188,   191,   194,   197,   200,   201,   202,   203,   204,   205,
     206,   207,   208
  };

  // Print the state stack on the debug stream.
  void
  conj_parser::yystack_print_ ()
  {
    *yycdebug_ << "Stack now";
    for (stack_type::const_iterator
           i = yystack_.begin (),
           i_end = yystack_.end ();
         i != i_end; ++i)
      *yycdebug_ << ' ' << i->state;
    *yycdebug_ << std::endl;
  }

  // Report on the debug stream that the rule \a yyrule is going to be reduced.
  void
  conj_parser::yy_reduce_print_ (int yyrule)
  {
    unsigned int yylno = yyrline_[yyrule];
    int yynrhs = yyr2_[yyrule];
    // Print the symbols being reduced, and their result.
    *yycdebug_ << "Reducing stack by rule " << yyrule - 1
               << " (line " << yylno << "):" << std::endl;
    // The symbols being reduced.
    for (int yyi = 0; yyi < yynrhs; yyi++)
      YY_SYMBOL_PRINT ("   $" << yyi + 1 << " =",
                       yystack_[(yynrhs) - (yyi + 1)]);
  }
#endif // YYDEBUG



} // yy
#line 2906 "conj.c" // lalr1.cc:1167
#line 209 "GrammarConfig.y" // lalr1.cc:1168


//body of the is_pure function for the expression struct above
//is_pure indicates whether the expression can be duplicated or deleted without changing program behaviour
bool expression::is_pure() const
{
	for(const auto &e : params)
	{
		if(!e.is_pure()) return false;
		switch(type)
		{
			//function calls are assumed not to be pure - some will be, but this is complicated to work out
			case ex_type::fcall:
			//assignment is not pure
			case ex_type::copy:
			//returns are not pure as they do not evaluate to a value;
			case ex_type::ret:
			//loops are not pure as they could be infinite - deleting this loop would stop the program from functioning properly altogether
			case ex_type::loop: return false;
			//anything else is pure
			default: true;
		}
	}
}
