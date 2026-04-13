// 1. PC Chairs can do anything with papers, reviews and meta-reviews as long as they are not an author of that paper
permit (principal, action, resource is Paper)
when {principal.isPCChair && !(principal in resource.authors)};

permit (principal, action, resource is Review)
when {principal.isPCChair && !(principal in resource.ofPaper.authors)};

// 2. Area Chairs can read, delete, update reviews for any paper as long as they
//    are not an author of that paper

// 3. Area Chairs can read, delete, update meta-reviews for any paper as long as
//    they are not an author of that paper

// 4. Area Chairs can make any review or meta-review visible to authors for any
//    paper as long as they are not an author of that paper

permit
  ( principal is User
  , action in [Action::"Read", Action::"Update", Action::"Delete", Action::"ReleaseReview"]
  , resource is Review )
when
  {     principal.isAreaChair
    && !(principal in resource.ofPaper.authors) };

// permit (principal, action, resource is Paper)
// when {principal.isAreaChair && !(principal in resource.authors)};

// permit (principal, action in [Action::"Read", Action::"Update", Action::"Delete"], resource is Review)
// when {principal.isAreaChair && !(principal in resource.ofPaper.authors)};

// permit (principal, action in Action::"ReleaseReview", resource is Review)
// when {principal.isAreaChair && !(principal in resource.ofPaper.authors)};

// 5. PC members can read reviews for any paper as long as they are not an author of that paper  
permit (principal, action in Action::"Read", resource is Review)
when {principal has pcMember && !(principal in resource.ofPaper.authors)};

// 6. PC members can read all papers
permit (principal, action in Action::"Read", resource is Paper)
when { principal has pcMember };

// 7 [DELETED]

// 8. PC members can only update reviews of the papers to which they are assigned
// invariant: in reviewers of some paper -> pc member (conference has no external reviewers)
permit (principal, action in Action::"Update", resource is Review)
when {principal in resource.ofPaper.reviewers};

// 9. Authors can read, update, delete papers they have written
permit (principal, action, resource is Paper)
when {principal in resource.authors};

// 10. Authors can read reviews and meta-reviews of papers they are authors of
permit (principal, action in Action::"Read", resource is Review)
when {principal in resource.ofPaper.authors};
