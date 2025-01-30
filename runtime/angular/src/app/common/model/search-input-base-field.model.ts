import { SortDirectionEnum } from './sort-direction.enum';
import { FilterOperatorEnum } from './filter-operator.enum';

/**
 * Class that holds all necessary data for filtering data for one field in the back-end
 */
export class SearchInputBaseField {
  name = '';
  dateTimeFormat = false;
  sortOrder = SortDirectionEnum.NONE;
  operator = FilterOperatorEnum.EQUAL;
  filterCriteria: string | null = null;

}
