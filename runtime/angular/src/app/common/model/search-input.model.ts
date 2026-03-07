import { DatePipe } from '@angular/common';
import { FilterOperatorEnum } from './filter-operator.enum';
import { FieldTypeEnum } from './field-type.enum';
import { AbstractSearchInput } from './abstract-search-input.model';
import { SearchInputField } from './search-input-field.model';
import { DateConverter } from '../converter/date-converter';

/**
 * Class that contains all properties that are necessary for building a dynamic search input form.
 * Instances of this class are also used to send their state to the backend in order to perform a
 * search operation.
 */
export class SearchInput extends AbstractSearchInput {
  searchFields: SearchInputField[] = [];

  /**
   * Return all supported filter operators for numeric fields
   */
  static getNumericOperators(): FilterOperatorEnum[] {
    return [FilterOperatorEnum.EQUAL, FilterOperatorEnum.GREATER, FilterOperatorEnum.SMALLER,
      FilterOperatorEnum.GREATER_OR_EQUAL, FilterOperatorEnum.SMALLER_OR_EQUAL,
      FilterOperatorEnum.BETWEEN, FilterOperatorEnum.IN, FilterOperatorEnum.NOT_IN,
      FilterOperatorEnum.IS_NULL, FilterOperatorEnum.IS_NOT_NULL];
  }

  /**
   * Return all supported filter operators for string fields
   */
  static getStringOperators(): FilterOperatorEnum[] {
    return [FilterOperatorEnum.LIKE, FilterOperatorEnum.EQUAL, FilterOperatorEnum.NOT_LIKE,
      FilterOperatorEnum.IS_NULL, FilterOperatorEnum.IS_NOT_NULL];
  }

  /**
   * Return all supported filter operators for enumeration fields
   */
  static getEnumOperators(): FilterOperatorEnum[] {
    return [FilterOperatorEnum.EQUAL, FilterOperatorEnum.NOT_LIKE];
  }

  /**
   * Return all supported filter operators for boolean fields
   */
  static getBooleanOperators(): FilterOperatorEnum[] {
    return [FilterOperatorEnum.EQUAL, FilterOperatorEnum.IS_NULL, FilterOperatorEnum.IS_NOT_NULL];
  }

  /**
   * Return all supported filter operators for date fields
   */
  static getDateOperators(): FilterOperatorEnum[] {
    return [FilterOperatorEnum.EQUAL, FilterOperatorEnum.GREATER, FilterOperatorEnum.SMALLER,
      FilterOperatorEnum.GREATER_OR_EQUAL, FilterOperatorEnum.SMALLER_OR_EQUAL,
      FilterOperatorEnum.IS_NULL, FilterOperatorEnum.IS_NOT_NULL];
  }

  /**
   * Return all supported filter operators for UUID fields
   */
  static getUUIDOperators(): FilterOperatorEnum[] {
    return [FilterOperatorEnum.EQUAL, FilterOperatorEnum.IS_NULL, FilterOperatorEnum.IS_NOT_NULL];
  }

  /**
   * Search a field by its name.
   * The method will return 'undefined' if the field could not be found!
   */
  getSearchFieldByName(name: string): SearchInputField | undefined {
    return this.searchFields.find(item => item.name === name);
  }

  /**
   * Create a new search field by using the provided parameters and add it to the search object
   */
  addSearchField(name: string, label: string, fieldType: FieldTypeEnum, width: number): SearchInputField {
    const searchField = new SearchInputField();
    searchField.name = name;
    searchField.label = label;
    searchField.type = fieldType;
    searchField.width = width;

    if (searchField.type === FieldTypeEnum.DATE) {
      searchField.dateTimeFormat = true;
    }

    if (searchField.type === FieldTypeEnum.STRING) {
      searchField.operator = FilterOperatorEnum.LIKE;
    }

    this.searchFields.push(searchField);

    return searchField;
  }

  /**
   * If no data is entered into a filter field the data binding will automatically write an empty
   * string into field 'filterCriteria'. The backend tries to interpret these values which is
   * basically unintended and is likely to cause parsing errors. Thus, the empty strings must be
   * replaced by using null! Furthermore, it is necessary to change the filter criteria of date
   * fields.
   */
  prepareFilterCriteria(locale: string) {
    this.searchFields.forEach(field => {
      if (field.filterCriteria === '') {
        field.filterCriteria = null;
      }

      // Date values must be converted by using a format that can be handled by the backend!
      if (field.type === FieldTypeEnum.DATE && field.dateCriterion !== null) {
        const datePipe = new DatePipe(locale);
        const dateValue = DateConverter.convertToDate(new Date(field.dateCriterion));

        if (field.dateTimeFormat === true) {
          field.filterCriteria = datePipe.transform(dateValue, SearchInput.DATE_TIME_FORMAT);
        } else {
          field.filterCriteria = datePipe.transform(dateValue, SearchInput.DATE_FORMAT);
        }
      }
    });

    return this;
  }

}
