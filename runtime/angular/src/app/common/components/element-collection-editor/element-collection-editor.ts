import { Component, forwardRef, Input, OnInit } from '@angular/core';
import { ConfirmationService, MenuItem, MessageService } from 'primeng/api';
import { FormBuilder, NG_VALUE_ACCESSOR, ControlValueAccessor } from '@angular/forms';
import { I18NService } from '../../services/i18n.service';
import { ValueConverter } from '../../converter/value-converter';
import { ValueType } from '../../converter/value-type.enum';

/**
 * Component for maintaining element collections
 */
@Component({
  selector: 'cc-element-collection-editor',
  templateUrl: './element-collection-editor.html',
  providers: [{
     provide: NG_VALUE_ACCESSOR,
     useExisting: forwardRef(() => ElementCollectionEditor),
     multi: true
   }]
})
export class ElementCollectionEditor<T extends string | Date | number > implements OnInit, ControlValueAccessor {
  @Input() uniqueElements = false;
  @Input() readOnly = false;
  @Input() fieldType = '';

  private onChange: (value: T[]) => void = () => {};

  valueType: ValueType = ValueType.STRING;
  elements: T[] = [];
  contextMenuItems: MenuItem[] = [];
  newValue: string = '';
  selectedItem: T | null = null;

  /**
   * Create a new instance
   */
  constructor(protected messageService: MessageService, protected confirmationService: ConfirmationService,
    protected formBuilder: FormBuilder, protected valueConverter: ValueConverter, protected i18n: I18NService) {
  }

  /**
   * Initialize the component
   */
  ngOnInit(): void {
    this.valueType = ValueType[this.fieldType as keyof typeof ValueType];

    const initialValue = this.valueConverter.getInitialDefaultValue(this.valueType);

    if (initialValue) {
      this.newValue = initialValue;
    }

    if (!this.readOnly) {
      this.addContextMenuItem(this.i18n.translate('action_delete'), 'pi pi-trash', 'mniDelete', () => this.deleteSelectedElement());
      this.addContextMenuItem(this.i18n.translate('action_delete_all'), 'pi pi-trash', 'mniDeleteAll', () => this.deleteAllElements());
    }
  }

  /**
   * Write the value
   */
   writeValue(value: T[]) {
     if (value !== this.elements) {
       this.elements = value;
     }
   }

  /**
   * Callback function that is called when the control's value changes in the UI
   */
  registerOnChange(fn: (value: T[]) => void) {
    this.onChange = fn;
  }

  /**
   * Callback function that is called by the forms API on initialization to update the form model on blur
   */
  registerOnTouched() {
    // No implementation required!
  }

  /**
   * Get the items to be displayed in the data table. The text in the field for adding new values is used as filter!
   */
  get filteredElements(): T[] {
    if (!this.newValue || this.newValue === this.valueConverter.getInitialDefaultValue(this.valueType)) {
      return this.elements.sort();
    }

    return this.elements.filter(element => {
      const elementString = this.convertElementToString(element);

      if (!elementString) {
        return false;
      }

      return elementString.startsWith(this.newValue);
    }).sort();
  }

  /**
   * Add a new element to the collection
   */
  addElement(): void {
    if (!this.newValue) {
      return;
    }

    try {
      let newElement = this.valueConverter.convertToValue(this.newValue, this.valueType);

      if (typeof newElement === 'string' && this.valueType === ValueType.CHARACTER) {
        newElement = newElement.substring(0, 1);
      }

      // Do not allow to add duplicate elements if the list is bound to a set
      if (this.uniqueElements && this.elements.includes(newElement)) {
        return;
      }

      this.elements.push(newElement);
      this.newValue = '';

      this.onChange(this.elements);
    } catch (error) {
      if (error instanceof Error) {
        this.messageService.add({ severity: 'warn', summary: error.message });
      }
    }
  }

  /**
   * Convert the given value to a string
   */
  convertElementToString(value: string | Date | number | null) {
    return this.valueConverter.convertToString(value, this.valueType);
  }

  /**
   * Delete the selected element
   */
  deleteSelectedElement(): void {
    if (this.selectedItem) {
      this.elements = this.elements.filter(item => item !== this.selectedItem);
      this.onChange(this.elements);
    }
  }

  /**
   * Delete all elements
   */
  deleteAllElements(): void {
    this.elements = [];
    this.onChange(this.elements);
  }

  /**
   * Callback method that keeps track of the selected element
   */
  onRowSelect(event: any): void {
    this.selectedItem = event.data;
  }

  /**
   * Add an item to the context menu
   */
  addContextMenuItem(label: string, icon: string, id: string, command: () => void) {
    const menuItem = { label: label, icon: icon, id: id, command: command };

    this.contextMenuItems.push(menuItem);
  }
}
