import { Component, Input, Output, EventEmitter, OnInit } from '@angular/core';
import { AbstractDataTable } from '../abstract-data-table/abstract-data-table';
import { MessageService, ConfirmationService } from 'primeng/api';
import { I18NService } from '../../services/i18n.service';
import { FormatterService } from '../../services/formatter.service';

/**
 * Abstract base class for all list-of-values dialogs
 */
@Component({ template: ''})
export abstract class AbstractListOfValuesDialog<T> extends AbstractDataTable<T> implements OnInit {
  private static readonly DEFAULT_INPUT_LENGTH = 1;
  private static readonly DEFAULT_DELAY = 200;
  private static readonly LOV_DEFAULT_MAX_NUMBER_OF_ITEMS = 100;

  @Output() closeDialog = new EventEmitter();
  @Input() visible = false;
  @Input() enableReset = false;
  @Input() minLength = 0;
  @Input() delay = 0;
  timeout: ReturnType<typeof setTimeout> | null = null;
  lastInput: string | null = null;

  /**
   * Create a new instance
   */
  constructor(protected override confirmationService: ConfirmationService,
    protected override messageService: MessageService,
    protected override i18n: I18NService,
    protected override formatterService: FormatterService) {
    super(confirmationService, messageService, i18n, formatterService);
  }

  /**
   * Initialize internal properties
   */
  ngOnInit() {
    this.minLength = AbstractListOfValuesDialog.DEFAULT_INPUT_LENGTH;
    this.delay = AbstractListOfValuesDialog.DEFAULT_DELAY;
    this.maxNumberOfItems = AbstractListOfValuesDialog.LOV_DEFAULT_MAX_NUMBER_OF_ITEMS;
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
   * Search items that should be displayed in the table by using the provided input
   */
  performSearchOperation(filterInput: string) {
    if (this.timeout) {
      clearTimeout(this.timeout);
    }

    if (filterInput.length === 0) {
      this.items = [];
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
      super.searchItems(filterInput);
    }, this.delay);
  }

  /**
   * Notify the parent component that this dialog will be closed
   */
  close() {
    this.closeDialog.emit();
  }

}
