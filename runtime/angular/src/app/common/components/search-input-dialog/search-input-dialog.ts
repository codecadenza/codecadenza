import { Component, OnInit, Input, Output, EventEmitter } from '@angular/core';
import { Validators, UntypedFormGroup, UntypedFormControl, AbstractControl, UntypedFormBuilder, UntypedFormArray, ValidatorFn } from '@angular/forms';
import { MessageService, SelectItem } from 'primeng/api';
import { SelectionItem } from '../../model/selection-item.model';
import { SearchInput } from '../../model/search-input.model';
import { FieldTypeEnum } from '../../model/field-type.enum';
import { SortDirectionEnum } from '../../model/sort-direction.enum';
import { FilterOperatorEnum } from '../../model/filter-operator.enum';
import { NumberConverter } from '../../converter/number-converter';
import { I18NService } from '../../services/i18n.service';

/**
 * This component provides a dialog with a dynamic form for entering filter and sorting criteria
 */
@Component({
  selector: 'cc-search-input-dialog',
  templateUrl: './search-input-dialog.html'
})
export class SearchInputDialog implements OnInit {
  private static readonly IN_DELIMITER = ';;';
  private static readonly BETWEEN_DELIMITER = '  ';
  private static readonly INTEGER_REGEX = RegExp(/^-?\d+$/);

  @Input() searchInput!: SearchInput;
  @Input() visible = false;
  @Output() performSearch = new EventEmitter();
  @Output() performCount = new EventEmitter();
  @Output() closeDialog = new EventEmitter();
  FieldType = FieldTypeEnum;
  searchFieldFormArray!: UntypedFormArray;
  fetchSizeValues = [10, 50, 100, 500, 1000, 5000, 10000];
  booleanValues = ['', 'true', 'false'];
  sortOrders = [SortDirectionEnum.NONE, SortDirectionEnum.ASC, SortDirectionEnum.DESC];
  searchInputForm!: UntypedFormGroup;
  sortOrderItems: SelectItem[] = [];
  stringOperatorItems: SelectItem[] = [];
  enumOperatorItems: SelectItem[] = [];
  booleanOperatorItems: SelectItem[] = [];
  dateOperatorItems: SelectItem[] = [];
  numericOperatorItems: SelectItem[] = [];
  uuidOperatorItems: SelectItem[] = [];
  fetchSizeItems: SelectItem[] = [];
  booleanValueItems: SelectItem[] = [];

  /**
   * Create a new instance
   */
  constructor(protected formBuilder: UntypedFormBuilder, protected numberConverter: NumberConverter,
    protected messageService: MessageService, protected i18n: I18NService) {
  }

  /**
   * Callback listener that is triggered as soon as the component should be initialized
   */
  ngOnInit() {
    this.initDialog();
  }

  /**
   * Initialize the dialog
   */
  initDialog() {
    console.log('Initialize search input dialog');

    this.initControls();

    this.searchFieldFormArray = new UntypedFormArray([]);

    // Create a nested form group for every search input field
    this.searchInput.searchFields.forEach(field => {
      if (field.type === FieldTypeEnum.DATE && field.dateCriterion !== null) {
        field.dateCriterion = new Date(field.dateCriterion);
      }

      this.searchFieldFormArray.push(this.createSearchFieldFormGroup(field.type));
    });

    this.searchInputForm = this.formBuilder.group({
      maxResult: new UntypedFormControl('', Validators.required),
      caseSensitive: new UntypedFormControl(''),
      exactFilterMatch: new UntypedFormControl(''),
      dateFormat: new UntypedFormControl(''),
      dateTimeFormat: new UntypedFormControl(''),
      numberFormat: new UntypedFormControl(''),
      searchFields: this.searchFieldFormArray
    });

    this.searchInputForm.patchValue(this.searchInput);
  }

  /**
   * Initialize all drop-down lists
   */
  initControls() {
    this.sortOrderItems = [];
    this.stringOperatorItems = [];
    this.enumOperatorItems = [];
    this.numericOperatorItems = [];
    this.booleanOperatorItems = [];
    this.dateOperatorItems = [];
    this.booleanValueItems = [];
    this.fetchSizeItems = [];

    this.sortOrders.forEach(sortOrder => {
      this.sortOrderItems.push(this.convertToSelectItemOfString(sortOrder));
    });

    SearchInput.getStringOperators().forEach(op => {
      this.stringOperatorItems.push(this.convertToSelectItemOfString(op));
    });

    SearchInput.getEnumOperators().forEach(op => {
      this.enumOperatorItems.push(this.convertToSelectItemOfString(op));
    });

    SearchInput.getNumericOperators().forEach(op => {
      this.numericOperatorItems.push(this.convertToSelectItemOfString(op));
    });

    SearchInput.getBooleanOperators().forEach(op => {
      this.booleanOperatorItems.push(this.convertToSelectItemOfString(op));
    });

    SearchInput.getUUIDOperators().forEach(op => {
      this.uuidOperatorItems.push(this.convertToSelectItemOfString(op));
    });

    SearchInput.getDateOperators().forEach(op => {
      this.dateOperatorItems.push(this.convertToSelectItemOfString(op));
    });

    this.booleanValues.forEach(value => {
      this.booleanValueItems.push(this.convertToSelectItemOfString(value));
    });

    this.fetchSizeValues.forEach(value => {
      this.fetchSizeItems.push(this.convertToSelectItemOfNumber(value));
    });
  }

  /**
   * Create a form group for a search input field
   */
  createSearchFieldFormGroup(fieldType: FieldTypeEnum) {
    return new UntypedFormGroup({
      filterCriteria: this.createFilterCriteriaField(fieldType),
      dateCriterion: new UntypedFormControl(''),
      label: new UntypedFormControl(''),
      sortOrder: new UntypedFormControl(''),
      operator: new UntypedFormControl(''),
      name: new UntypedFormControl(''),
      type: new UntypedFormControl(''),
      displayInDialog: new UntypedFormControl(''),
      displayInTable: new UntypedFormControl(''),
      dateTimeFormat: new UntypedFormControl(''),
      selectionList: new UntypedFormControl('')
    });
  }

  /**
   * Create a filter criteria field with a specific validator depending on the given field type
   */
  createFilterCriteriaField(fieldType: FieldTypeEnum): UntypedFormControl {
    if (fieldType === FieldTypeEnum.INTEGER) {
      return new UntypedFormControl('', [this.createIntegerValidator()]);
    } else if (fieldType === FieldTypeEnum.DECIMAL) {
      return new UntypedFormControl('', [this.createDecimalValidator()]);
    }

    return new UntypedFormControl('');
  }

  /**
   * Create a validator that only allows decimal values
   */
  createDecimalValidator(): ValidatorFn {
    return (control: AbstractControl): { [key: string]: boolean } | null => {
      let value = control.value;

      try {
        if (!control.pristine && value && typeof value === 'string') {
          const filterOperator = control.parent?.get('operator')?.value;

          if (filterOperator === FilterOperatorEnum.IN || filterOperator === FilterOperatorEnum.NOT_IN) {
            const numericValues = value.split(SearchInputDialog.IN_DELIMITER);

            numericValues.forEach(numberPart => {
              value = numberPart;
              this.numberConverter.convertToNumber(value);
            });
          } else if (filterOperator === FilterOperatorEnum.BETWEEN) {
            const numericValues = value.split(SearchInputDialog.BETWEEN_DELIMITER);

            if (!control.errors && numericValues.length !== 2) {
              const message = { severity: 'warn', summary: this.i18n.translate('msg_errorbetween') };
              this.messageService.add(message);
              return { 'invalidDecimal': true };
            }

            numericValues.forEach(numberPart => {
              value = numberPart;
              this.numberConverter.convertToNumber(value);
            });
          } else {
            this.numberConverter.convertToNumber(value);
          }
        }

        return null;
      } catch(error) {
        if (!control.errors) {
          const message = { severity: 'warn', summary: this.i18n.translate('msg_errorconvertdecimal', value) };
          this.messageService.add(message);
        }

        return { 'invalidDecimal': true };
      }
    };
  }

  /**
   * Create a validator that only allows integer values
   */
  createIntegerValidator(): ValidatorFn {
    return (control: AbstractControl): { [key: string]: boolean } | null => {
      const filterOperator = control.parent?.get('operator')?.value;
      const value = control.value;

      try {
        if (!control.pristine && value && typeof value === 'string') {
          if (filterOperator === FilterOperatorEnum.IN || filterOperator === FilterOperatorEnum.NOT_IN) {
            const numericValues = value.split(SearchInputDialog.IN_DELIMITER);

            numericValues.forEach(numberPart => {
              if (!SearchInputDialog.INTEGER_REGEX.test(numberPart)) {
                throw Error(this.i18n.translate('msg_errorconvertinteger', numberPart));
              }
            });
          } else if (filterOperator === FilterOperatorEnum.BETWEEN) {
            const numericValues = value.split(SearchInputDialog.BETWEEN_DELIMITER);

            if (numericValues.length !== 2) {
              throw Error(this.i18n.translate('msg_errorbetween'));
            }

            numericValues.forEach(numberPart => {
              if (!SearchInputDialog.INTEGER_REGEX.test(numberPart)) {
                throw Error(this.i18n.translate('msg_errorconvertinteger', numberPart));
              }
            });
          } else if (!SearchInputDialog.INTEGER_REGEX.test(value)) {
            throw Error(this.i18n.translate('msg_errorconvertinteger', value));
          }
        }

        return null;
      } catch(error) {
        if (!control.errors && error instanceof Error) {
          const message = { severity: 'warn', summary: error.message };
          this.messageService.add(message);
        }

        return { 'invalidInteger': true };
      }
    };
  }

  /**
   * Check if the respective filter criteria field is valid after changing the filter operator
   */
  onOperatorChange(control: AbstractControl) {
    control.get('filterCriteria')?.updateValueAndValidity();
  }

  /**
   * Reset the search input object
   */
  resetSearchInput() {
    const initSearchInput = new SearchInput();
    this.searchInput.maxResult = initSearchInput.maxResult;
    this.searchInput.exactFilterMatch = initSearchInput.exactFilterMatch;
    this.searchInput.caseSensitive = initSearchInput.caseSensitive;
    this.searchInput.startIndex = initSearchInput.startIndex;

    this.searchInput.searchFields.forEach(field => {
      // Reset all relevant fields
      field.displayInTable = true;
      field.filterCriteria = null;
      field.dateCriterion = null;
      field.sortOrder = SortDirectionEnum.NONE;
      field.operator = FilterOperatorEnum.EQUAL;

      if (field.type === FieldTypeEnum.STRING || field.type === FieldTypeEnum.UUID) {
        field.operator = FilterOperatorEnum.LIKE;
      }
    });

    this.searchInputForm.patchValue(this.searchInput);
  }

  /**
   * Apply the entered form data to the search input object and notify the parent component
   * to perform a search operation
   */
  applyInput() {
    this.visible = false;

    this.searchInput = new SearchInput();

    Object.assign(this.searchInput, this.searchInputForm.value);

    this.performSearch.emit(this.searchInput.prepareFilterCriteria(this.numberConverter.getLocale()));
  }

  /**
   * Apply the entered form data to the search input object and notify the parent component
   * to perform a count operation
   */
  countRecords() {
    this.searchInput = new SearchInput();

    Object.assign(this.searchInput, this.searchInputForm.value);

    this.performCount.emit(this.searchInput.prepareFilterCriteria(this.numberConverter.getLocale()));
  }

  /**
   * Notify the parent component that this dialog will be closed
   */
  close() {
    this.closeDialog.emit();
  }

  /**
   * Check if the given search input field should be displayed
   */
  displaySearchInputField(control: AbstractControl): boolean {
    const field = control.get('displayInDialog');

    if (field == null) {
      return false;
    }

    return field.value === true;
  }

  /**
   * Get the label of the given search input field
   */
  getSearchInputFieldLabel(control: AbstractControl): string {
    const field = control.get('label');

    if (field == null) {
      return '';
    }

    return field.value;
  }

  /**
   * Get the type of the search input field
   */
  getSearchInputFieldType(control: AbstractControl): FieldTypeEnum {
    const field = control.get('type');

    if (field == null) {
      return FieldTypeEnum.STRING;
    }

    return field.value;
  }

  /**
   * Check if a calendar control of a search input field should display a control for selecting the time
   */
  showTime(control: AbstractControl): boolean {
    const field = control.get('dateTimeFormat');

    if (field == null) {
      return false;
    }

    return field.value;
  }

  /**
   * Get the selection list of a search input field
   */
  getSelectionList(control: AbstractControl): SelectionItem[] {
    const field = control.get('selectionList');

    if (field == null) {
      return [];
    }

    return field.value;
  }

  /**
   * Convert the given value into a SelectItem with a string value
   */
  private convertToSelectItemOfString(value: string) {
    return { label: value, value: value };
  }

  /**
   * Convert the given value into a SelectItem with a number value
   */
  private convertToSelectItemOfNumber(value: number) {
    return { label: value.toString(), value: value };
  }

}
