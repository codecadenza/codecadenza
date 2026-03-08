import { AbstractSearchInput } from './abstract-search-input.model';
import { SearchInputBaseField } from './search-input-base-field.model';
import { SearchInput } from './search-input.model';
/**
 * Class that contains all properties that are necessary for searching data in the backend
 */
export class SearchInputBackend extends AbstractSearchInput {
  searchFields: SearchInputBaseField[] = [];

  /**
   * Convert the given search input object to a form that can be processed in the backend
   */
  static convert(searchInput: SearchInput, decimalSeparator: string, groupingSeparator: string): SearchInputBackend {
    const searchInputBackend = new SearchInputBackend();
    searchInputBackend.decimalSeparator = decimalSeparator;
    searchInputBackend.groupingSeparator = groupingSeparator;
    searchInputBackend.caseSensitive = searchInput.caseSensitive;
    searchInputBackend.exactFilterMatch = searchInput.exactFilterMatch;
    searchInputBackend.maxResult = searchInput.maxResult;

    searchInput.searchFields.forEach(searchInputField => {
      const backendField = new SearchInputBaseField();
      backendField.dateTimeFormat = searchInputField.dateTimeFormat;
      backendField.filterCriteria = searchInputField.filterCriteria;
      backendField.name = searchInputField.name;
      backendField.operator = searchInputField.operator;
      backendField.sortOrder = searchInputField.sortOrder;

      searchInputBackend.searchFields.push(backendField);
    });

    return searchInputBackend;
  }

}
