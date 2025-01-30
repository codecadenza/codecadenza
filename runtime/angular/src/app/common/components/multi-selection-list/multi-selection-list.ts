import { Component, Input, forwardRef, Output, EventEmitter } from '@angular/core';
import { NG_VALUE_ACCESSOR, ControlValueAccessor } from '@angular/forms';
import { I18NService } from '../../services/i18n.service';

/**
 * Component to select any number of items from a source list
 */
@Component({
  selector: 'cc-multi-selection-list',
  templateUrl: './multi-selection-list.html',
  providers: [{
    provide: NG_VALUE_ACCESSOR,
    useExisting: forwardRef(() => MultiSelectionList),
    multi: true
  }]
})
// eslint-disable-next-line @typescript-eslint/no-explicit-any
export class MultiSelectionList<T extends { [key: string]: any; }> implements ControlValueAccessor {
  private static readonly DEFAULT_INPUT_LENGTH = 2;
  private static readonly DEFAULT_DELAY = 200;
  private static readonly DEFAULT_MAX_NUMBER_OF_ITEMS = 100;

  @Output() filterInputChanged = new EventEmitter<string>();
  @Input() labelFieldName = '';
  @Input() delay: number;
  @Input() minLength: number;
  @Input() filterSourceItems = true;
  @Input() maxNumberOfItems: number;
  @Input() disabled = false;
  selectedItems: T[] = [];
  loading = false;
  timeout: ReturnType<typeof setTimeout> | null = null;
  lastInput: string | null = null;
  sourceListHeader = '';
  targetListHeader = '';
  _availableItems: T[] = [];

  /**
   * Initialize internal properties
   */
  constructor(protected i18n: I18NService) {
    this.minLength = MultiSelectionList.DEFAULT_INPUT_LENGTH;
    this.delay = MultiSelectionList.DEFAULT_DELAY;
    this.maxNumberOfItems = MultiSelectionList.DEFAULT_MAX_NUMBER_OF_ITEMS;
  }

  /**
   * Write the value
   */
  writeValue(value: T[]) {
    if (value !== this.selectedItems) {
      this.selectedItems = value;
    }
  }

  /**
   * Callback function that is called when the control's value changes in the UI
   */
  registerOnChange() {
    this.updateListHeaders();
  }

  /**
   * Update the headers of the target and the source list
   */
  updateListHeaders() {
    this.targetListHeader = this.i18n.translate('multiselectionlist_targetheader', this.selectedItems.length.toString());
    this.sourceListHeader = this.i18n.translate('multiselectionlist_sourceheader', this._availableItems.length.toString());
  }

  /**
   * Callback function that is called by the forms API on initialization to update the form model on blur
   */
  registerOnTouched() {
    // No implementation required!
  }

  /**
   * Callback listener that is triggered as soon as a user enters a key in the search field
   */
  onKeyUp($event: KeyboardEvent) {
    if ($event.target instanceof HTMLInputElement) {
      this.performSearchOperation($event.target.value);
    }
  }

  /**
   * Search items that should be displayed in the source list by using the provided input
   */
  performSearchOperation(filterInput: string) {
    if (!this.filterSourceItems) {
      return;
    }

    if (this.timeout) {
      clearTimeout(this.timeout);
    }

    if (filterInput.length === 0) {
      this._availableItems = [];
      this.lastInput = null;
      return;
    }

    if (filterInput.length < this.minLength) {
      this.lastInput = null;
      return;
    }

    if (this.loading) {
      // In case of a slow back-end we save the input in order to stay in sync!
      this.lastInput = filterInput;
      return;
    } else {
      this.lastInput = null;
    }

    // If a last input exists we will ignore the current filter string!
    if (this.lastInput != null) {
      filterInput = this.lastInput;
    }

    this.timeout = setTimeout(() => {
      this.loading = true;
      this.filterInputChanged.emit(filterInput);
    }, this.delay);
  }

  /**
   * Return the text that should be displayed for an item
   */
  getItemText(item: T): string {
     const label = item[this.labelFieldName];

    if (typeof label === 'string') {
      return label;
    } else if (typeof label === 'number') {
      return label.toString();
    }

    throw Error('Unsupported type for label field: "' + this.labelFieldName + '"');
  }

  /**
   * Return an array that contains all available items
   */
  @Input() get availableItems() {
    return this._availableItems;
  }

  /**
   * Add the items of the given array to the list of available items
   */
  set availableItems(items: Array<T>) {
    this._availableItems = [];

    if (!items) {
      this.updateListHeaders();
      this.loading = false;
      return;
    }

    // Do not display items in the source list that are already contained in the target list!
    for (const availableItem of items) {
      if (this._availableItems.length === this.maxNumberOfItems) {
        break;
      }

      const item = this.selectedItems.find(selectedItem => this.getItemText(selectedItem) === this.getItemText(availableItem));

      if (!item) {
        this._availableItems.push(availableItem);
      }
    }

    this.updateListHeaders();
    this.loading = false;

    // If a last input exists we will start once again in order to stay in sync with the user's input!
    if (this.lastInput !== null) {
      this.performSearchOperation(this.lastInput);
      this.lastInput = null;
    }
  }

}
