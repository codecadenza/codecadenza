import { Component, Input, OnInit } from '@angular/core';
import { Validators, FormGroup, FormControl, ValidatorFn, AbstractControl } from '@angular/forms';
import { MessageService } from 'primeng/api';
import { SliderChangeEvent } from 'primeng/slider';
import { I18NService } from '../../services/i18n.service';
import { ComponentScalingService } from '../../services/component-scaling.service';
import { FormatterService } from '../../services/formatter.service';
import { NavigationHistoryService } from '../../services/navigation-history.service';
import { UserSettings } from '../../model/user-settings.model';

/**
 * Page for changing the user settings
 */
@Component({
  templateUrl: './user-settings-page.html'
})
export class UserSettingsPage implements OnInit {
  @Input()
  minFontSize = ComponentScalingService.MIN_FONT_SIZE;
  @Input()
  maxFontSize = ComponentScalingService.MAX_FONT_SIZE;
  formGroup!: FormGroup;
  settings!: UserSettings;

  /**
   * Create a new instance
   */
  constructor(protected i18n: I18NService, protected formatterService: FormatterService,
    protected componentScalingService: ComponentScalingService, protected messageService: MessageService,
    protected navigationHistoryService: NavigationHistoryService) {
  }

  /**
   * Initialize the form
   */
  ngOnInit() {
    this.formGroup = new FormGroup({
      dateFormat: new FormControl('', Validators.required),
      dateTimeFormat: new FormControl('', Validators.required),
      numberFormat: new FormControl('', [Validators.required, this.createNumberFormatValidator()]),
      fontSize: new FormControl(0, Validators.required)
    });

    this.settings = {
      dateFormat: this.formatterService.getDateFormat(),
      dateTimeFormat: this.formatterService.getDateTimeFormat(),
      numberFormat: this.formatterService.getNumberFormat(),
      fontSize: this.componentScalingService.getFontSize()
    };

    this.formGroup.patchValue(this.settings);
  }

  /**
   * Callback listener for changing the component scaling
   */
  onComponentScalingChanged($event: SliderChangeEvent) {
    if (!$event.value) {
      return;
    }

    this.componentScalingService.changeComponentScaling($event.value);
  }

  /**
   * Create a validator that checks if the number format is valid
   */
  createNumberFormatValidator(): ValidatorFn {
    return (control: AbstractControl): { [key: string]: boolean } | null => {
      const currentNumberFormat = this.formatterService.getNumberFormat();
      const value = control.value;

      try {
        if (!control.pristine && value && typeof value === 'string') {
          this.formatterService.setNumberFormat(value);
          this.formatterService.formatNumber(Math.random());
        }

        return null;
      } catch(error) {
        this.formatterService.setNumberFormat(currentNumberFormat);

        return { 'invalidDecimalFormat': true };
      }
    };
  }

  /**
   * Validate a control by using the given validator. The method will return true if the input is valid.
   */
  validateControl(controlName: string, validator: string): boolean {
    return !this.formGroup.controls[controlName].hasError(validator);
  }

  /**
   * Save the format preferences
   */
  saveSettings() {
    Object.assign(this.settings, this.formGroup.value);

    this.formatterService.setDateFormat(this.settings.dateFormat);
    this.formatterService.setDateTimeFormat(this.settings.dateTimeFormat);
    this.formatterService.saveFormatSettings();

    this.navigationHistoryService.navigateToPreviousPage();
  }

}
