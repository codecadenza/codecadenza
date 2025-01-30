import { AbstractSearchInput } from './abstract-search-input.model';
import { SearchInputBaseField } from './search-input-base-field.model';

/**
 * Class that contains all properties that are necessary for searching data in the back-end
 */
export class SearchInputBackend extends AbstractSearchInput {
  searchFields: SearchInputBaseField[] = [];

}
