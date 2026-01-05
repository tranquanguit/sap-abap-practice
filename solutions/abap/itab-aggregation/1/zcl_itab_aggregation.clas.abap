CLASS zcl_itab_aggregation DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES group TYPE c LENGTH 1.
    TYPES: BEGIN OF initial_numbers_type,
             group  TYPE group,
             number TYPE i,
           END OF initial_numbers_type,
           initial_numbers TYPE STANDARD TABLE OF initial_numbers_type WITH EMPTY KEY.

    TYPES: BEGIN OF aggregated_data_type,
             group   TYPE group,
             count   TYPE i,
             sum     TYPE i,
             min     TYPE i,
             max     TYPE i,
             average TYPE f,
           END OF aggregated_data_type,
           aggregated_data TYPE STANDARD TABLE OF aggregated_data_type WITH EMPTY KEY.

    METHODS perform_aggregation
      IMPORTING
        initial_numbers        TYPE initial_numbers
      RETURNING
        VALUE(aggregated_data) TYPE aggregated_data.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_itab_aggregation IMPLEMENTATION.
  METHOD perform_aggregation.
    " add solution here --> Normal way
*    LOOP AT initial_numbers ASSIGNING FIELD-SYMBOL(<ls_line>)
*      GROUP BY ( group = <ls_line>-group )
*      ASCENDING
*      ASSIGNING FIELD-SYMBOL(<group>).
*
*      DATA(ls_aggregated) = VALUE aggregated_data_type( group = <group>-group ).
*
*      LOOP AT GROUP <group> ASSIGNING FIELD-SYMBOL(<group_line>).
*        ls_aggregated-count  += 1.
*        ls_aggregated-sum    += <group_line>-number.
*        ls_aggregated-min    = COND #( WHEN ls_aggregated-min = 0 OR ls_aggregated-min > <group_line>-number THEN <group_line>-number ELSE ls_aggregated-min ).
*        ls_aggregated-max    = COND #( WHEN ls_aggregated-max < <group_line>-number THEN <group_line>-number ELSE ls_aggregated-max ).
*      ENDLOOP.
*
*      ls_aggregated-average = ls_aggregated-sum / ls_aggregated-count.
*
*      INSERT ls_aggregated INTO TABLE aggregated_data.
*    ENDLOOP.

    " --> Interesting way
*    aggregated_data = VALUE #(
*      FOR GROUPS value OF <line> IN initial_numbers GROUP BY <line>-group WITHOUT MEMBERS
*      ( CORRESPONDING #(
*          BASE ( value )
*          REDUCE zcl_itab_aggregation=>aggregated_data_type(
*            INIT init TYPE zcl_itab_aggregation=>aggregated_data_type
*            FOR wa
*            IN initial_numbers
*            WHERE ( group = value )
*            NEXT
*              init-group = wa-group
*              init-count += 1
*              init-sum += wa-number
*              init-min = COND #( WHEN init-min = 0 OR init-min > wa-number THEN wa-number ELSE init-min )
*              init-max = COND #( WHEN init-max < wa-number THEN wa-number ELSE init-max )
*              init-average = init-sum / init-count )
*        MAPPING
*          group = group
*          count = count
*          sum = sum
*          min = min
*          max = max
*          average = average
*        EXCEPT * )
*      )
*    ).

    " OMG way --> :((( --> only use on exercism
    DATA: ls_aggregated TYPE aggregated_data_type.

    DATA(lt_sorted) = initial_numbers.
    SORT lt_sorted BY group number.

    LOOP AT lt_sorted ASSIGNING FIELD-SYMBOL(<ls_line>).

      IF ls_aggregated-group <> <ls_line>-group.
        CLEAR ls_aggregated.
        ls_aggregated-group = <ls_line>-group.
        ls_aggregated-min   = <ls_line>-number. 
      ENDIF.

      ls_aggregated-count += 1.
      ls_aggregated-sum   += <ls_line>-number.

      IF <ls_line>-number < ls_aggregated-min.
        ls_aggregated-min = <ls_line>-number.
      ENDIF.

      IF <ls_line>-number > ls_aggregated-max.
        ls_aggregated-max = <ls_line>-number.
      ENDIF.

      AT END OF group.
        ls_aggregated-average = ls_aggregated-sum / ls_aggregated-count.
        INSERT ls_aggregated INTO TABLE aggregated_data.
      ENDAT.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
