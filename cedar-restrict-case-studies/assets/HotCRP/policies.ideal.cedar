// 11. PC Chairs can do anything with papers, reviews and meta-reviews as long as
//     they are not an author of that paper

permit (principal, action, resource is Paper)
when {principal.isPCChair && !(principal in resource.authors)};

permit (principal, action, resource is Review)
when {principal.isPCChair && !(principal in resource.ofPaper.authors)};

// 12. Area Chairs *in their area* can read, delete, update reviews for any paper
//     as long as they are not an author of that paper

// 13. Area Chairs *in their areas* can read, delete, update meta-reviews for any
//     paper as long as they are not an author of that paper

// 14. Area Chairs *in their areas* can make any review or meta-review visible to
//     authors for any paper as long as they are not an author of that paper

permit
  ( principal is User
  , action in [Action::"Read", Action::"Update", Action::"Delete", Action::"ReleaseReview"]
  , resource is Review )
when
  {     principal.isAreaChair
    && !(principal in resource.ofPaper.authors)
    && (principal has pcMember && principal.pcMember == resource.ofPaper.area) };

// permit (principal, action, resource is Paper)
// when {
//   principal.isAreaChair
//   && !(principal in resource.authors)
//   && (principal has pcMember && principal.pcMember == resource.area)
//   };

// permit
//   ( principal
//   , action in [Action::"Read", Action::"Update", Action::"Delete"]
//   , resource is Review)
// when {
//   principal.isAreaChair
//   && !(principal in resource.ofPaper.authors)
//   && (principal has pcMember && principal.pcMember == resource.ofPaper.area)
//   };

// permit (principal, action in Action::"ReleaseReview", resource is Review)
// when {
//   principal.isAreaChair
//   && !(principal in resource.ofPaper.authors)
//   && (principal has pcMember && principal.pcMember == resource.ofPaper.area)
// };

// *****************************************************************************
// 15. PC members *in their areas* can read reviews for any paper as long as
//     they are not an author of that paper
// *****************************************************************************

permit (principal, action in Action::"Read", resource is Review)
when {
  principal has pcMember
  && !(principal in resource.ofPaper.authors)
  && principal.pcMember == resource.ofPaper.area
  };

// *****************************************************************************
// 16. PC members *in their areas* can read all papers
// *****************************************************************************

permit (principal, action in Action::"Read", resource is Paper)
when { principal has pcMember && principal.pcMember == resource.area };

// 17. [DELETED]

// 18. PC members can only update reviews of the papers to which they are assigned
//     *and they are the authors of the review*
// invariant: in reviewers of some paper -> pc member (conference has no external reviewers)
permit (principal, action in Action::"Update", resource is Review)
when {
  principal in resource.ofPaper.reviewers
  && principal == resource.author
  };

// 19. Authors can read, update, delete papers they have written
permit (principal, action, resource is Paper)
when {principal in resource.authors};

// 20. Authors can read reviews and meta-reviews of papers they are authors of
//     *and the reviews have been release*
permit (principal, action in Action::"Read", resource is Review)
when {
  principal in resource.ofPaper.authors
  && context has isReleased
  && context.isReleased
  };
