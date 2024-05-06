/// ### Example
///
/// ```rust
/// use crate::utils::tests::test_each;
///
/// fn add(a: i8, b: i8) -> i8 { a + b }
///
/// struct TestCase {
///   input: (i8, i8),
///   expected: i8,
/// }
///
/// test_each!(
///   test_add,
///   |case: TestCase| {
///     assert_eq!(
///       add(case.input.0, case.input.1),
///       case.expected
///     );
///   },
///   "1" => TestCase { input: (1, 2), expected: 3, },
///   "2" => TestCase { input: (4, 4), expected: 8, },
/// );
/// ```
#[macro_export]
macro_rules! test_each {
  ($name:ident, $fn:expr, $($suffix:expr => $case:expr,)*) => {
    paste::item! {
      $(
        #[test]
        fn [< $name _ $suffix >]() {
          $fn($case);
        }
      )*
    }
  };
}
