import { Component, inject, OnInit } from '@angular/core';
import { Validators, FormGroup, FormControl, ValidatorFn, AbstractControl, FormsModule, ReactiveFormsModule } from '@angular/forms';
import { SliderChangeEvent, Slider } from 'primeng/slider';
import { ComponentScalingService } from '../../services/component-scaling.service';
import { FormatterService } from '../../services/formatter.service';
import { NavigationHistoryService } from '../../services/navigation-history.service';
import { UserSettings } from '../../model/user-settings.model';
import { ViewContainer } from '../../components/view-container/view-container.component';
import { FormContainerComponent } from '../../components/form-container/form-container.component';
import { FormControlContainerComponent } from '../../components/form-control-container/form-control-container.component';
import { Bind } from 'primeng/bind';
import { InputText } from 'primeng/inputtext';
import { Message } from 'primeng/message';
import { ButtonDirective } from 'primeng/button';

/**
 * Page for changing the user settings
 */
@Component({
  templateUrl: './user-settings-page.html',
  imports: [ViewContainer, FormsModule, ReactiveFormsModule, FormContainerComponent, FormControlContainerComponent, Bind,
    InputText, Message, Slider, ButtonDirective]
})
export class UserSettingsPage implements OnInit {
  private readonly formatterService = inject(FormatterService);
  private readonly componentScalingService = inject(ComponentScalingService);
  private readonly navigationHistoryService = inject(NavigationHistoryService);
  protected minFontSize = ComponentScalingService.MIN_FONT_SIZE;
  protected maxFontSize = ComponentScalingService.MAX_FONT_SIZE;
  protected formGroup!: FormGroup;
  protected settings!: UserSettings;

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
    return (control: AbstractControl): Record<string, boolean> | null => {
      const currentNumberFormat = this.formatterService.getNumberFormat();
      const value = control.value;

      try {
        if (!control.pristine && value && typeof value === 'string') {
          this.formatterService.setNumberFormat(value);
          this.formatterService.formatNumber(Math.random());
        }

        return null;
      } catch {
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
