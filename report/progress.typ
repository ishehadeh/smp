#import "@preview/charged-ieee:0.1.0": ieee

#let abstract = [
  We introduce the Howlite programming language.
  Where it's from, and where it's going.
]

#show: ieee.with(
  title: [The Howlite Programming Language - First Semester Progress],
  authors: (
    (
      name: "Ian Shehadeh",
      department: [Computer Science],
      organization: [St. Mary's College of Maryland],
      location: [St. Mary's City, Maryland],
      email: "irshehadeh@smcm.edu"
    ),
  ),
  bibliography: bibliography("refs.bib"),
)


= Introducing Howlite

The Howlite programming language is a small systems programming language aimed at embedded devices and very low-level performance critical code.

== Project Goals

= Language Design

= Compiler Architecture

