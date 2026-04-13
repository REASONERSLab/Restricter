// posting assignments
// tightening: only when (principal.role == Role::"Teacher")
permit
  ( principal
  , action == Action::"PostAssignment"
  , resource )
  when { principal in resource.course && principal.role == Role::"Teacher" };

// editing assignments
// tightening: only when not isTA
permit
  ( principal
  , action == Action::"EditAssignment"
  , resource )
  when { principal in resource.course && principal.role == Role::"Teacher" };

// grading assignments
permit
  ( principal
  , action == Action::"PostGrade"
  , resource )
  when { principal in resource.assignment.course };

// viewing grades: staff
// tightening: only staff in the course can view
permit
  ( principal is Staff
  , action == Action::"ViewGrade"
  , resource is Grade )
  when { principal in resource.assignment.course } ;

// viewing grades
permit
  ( principal is Student
  , action == Action::"ViewGrade"
  , resource is Grade )
  when { principal == resource.student };
