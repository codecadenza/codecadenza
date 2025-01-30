import { FieldTypeEnum } from './field-type.enum';
import { SearchInputBaseField } from './search-input-base-field.model';
import { SelectionItem } from './selection-item.model';

/**
 * Class that defines the data structure for a single search field that is displayed in a search input form.
 * It also provides data for creating a table column dynamically.
 */
export class SearchInputField extends SearchInputBaseField {
  label = '';
  type = FieldTypeEnum.STRING;
  width = 0;
  dateCriterion: Date | null = null;
  selectionList: SelectionItem[] = [];
  displayInDialog = true;
  displayInTable = true;

  /**
   * Add a selection item to this search field
   */
  addSelectionItem(value: string, label?: string) {
    if (!label) {
      this.selectionList.push({ value: value, label: value });
    } else {
      this.selectionList.push({ value: value, label: label });
    }
  }

}
