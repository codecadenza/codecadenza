import { Component } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { UntypedFormGroup, ValidatorFn, UntypedFormControl } from '@angular/forms';
import { Location } from '@angular/common';
import { NavigationHistoryService } from '../../services/navigation-history.service';
import { I18NService } from '../../services/i18n.service';
import { Observable } from 'rxjs';

/**
 * Base class for all single-record forms
 */
@Component({ template: ''})
// eslint-disable-next-line @typescript-eslint/no-explicit-any
export abstract class AbstractSingleRecordForm<T extends { [key: string]: any; }> {
  public readonly UUID_PATTERN = '[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{12}';
  public readonly OPTIONAL_UUID_PATTERN = '^$|' + this.UUID_PATTERN;

  id!: string | null;
  object!: T;
  formGroup!: UntypedFormGroup;
  leavePage = true;
  error: Error | null = null;

  /**
   * Initialize the form
   */
  constructor(protected route: ActivatedRoute, protected location: Location, protected router: Router,
    protected navigationHistoryService: NavigationHistoryService, protected i18n: I18NService) {
  }

  /**
   * Initialize this form. Per definition, the caller must provide the parameter 'id' in the route's snapshot.
   * The parameter value is interpreted as the ID of the object to be loaded!
   */
  initForm() {
    this.id = this.route.snapshot.paramMap.get('id');

    if (!this.id) {
      this.openErrorDialog(new Error(this.i18n.translate('msg_missingidparameter')), true);
      return;
    }

    console.log('Open page for object with ID ' + this.id);

    // Create the main form group
    this.formGroup = new UntypedFormGroup({});

    this._loadObject(this.id);
  }

  /**
   * Load the form's object
   */
  abstract loadObject(id: string): Observable<T>;

  /**
   * Add all controls to the form group
   */
  abstract addControls(): void;

  /**
   * Save the form's object
   */
  abstract saveObject(object: T): Observable<T | void>;

  /**
   * Return true if the given object may be displayed in this dialog
   */
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  isObjectAllowed(_object: T): boolean {
    return true;
  }

  /**
   * Load the object from the back-end
   */
  _loadObject(id: string) {
    this.loadObject(id).subscribe({
      next: result => this.object = result,
      error: error => {
        this.openErrorDialog(error, true);
      },
      complete: () => {
        if (!this.isObjectAllowed(this.object)) {
          this.openErrorDialog(new Error(this.i18n.translate('msg_noobjectpermission')), true);
          return;
        }

        console.log('Loaded object: ' + JSON.stringify(this.object));

        // Add controls
        this.addControls();

        // Apply the object to the form group
        this.formGroup.patchValue(this.object);
      }
    });
  }

  /**
   * Save the object. An error dialog will be opened if the save operation has failed.
   */
  save() {
    const object = {} as T;

    Object.assign(object, this.formGroup.value);

    console.log('Save object ' + JSON.stringify(object));

    this.saveObject(object).subscribe({
      next: result => {
        if (!result) {
          return;
        }

        this.object = result;
      },
      error: error => this.openErrorDialog(error, false),
      complete: () => {
        if (this.object) {
          const navigationTarget = this.getNavigationTargetAfterSave();

          if (navigationTarget) {
            this.router.navigate([navigationTarget]);
            return;
          }
        }

        this.goBack();
      }
    });
  }

  /**
   * Add a new control to the main form group
   */
  addControl(name: string, validators?: ValidatorFn | ValidatorFn[], disabled?: boolean, defaultValue?: unknown) {
    const formControl = new UntypedFormControl({ value: '', disabled: disabled });
    formControl.setValue(defaultValue);

    if (validators) {
      formControl.setValidators(validators);
    }

    this.formGroup.addControl(name, formControl);
  }

  /**
   * Open the error dialog
   */
  openErrorDialog(error: Error, leavePage: boolean) {
    this.error = error;
    this.leavePage = leavePage;
  }

  /**
   * Validate a control by using the given validator. The method will return true if the input is valid.
   */
  validateControl(formControlName: string, validator: string): boolean {
    const control = this.formGroup.controls[formControlName];

    // Test if the control exists
    if (!control) {
      console.error('Cannot validate control "' + formControlName + '" as it does not belong to this form group!');
      return false;
    }

    if (control.pristine) {
      return true;
    }

    return !control.hasError(validator);
  }

  /**
   * Return the relative path to navigate to after the save operation has been finished successfully
   */
  getNavigationTargetAfterSave(): string | null {
    return null;
  }

  /**
   * Navigate back to the previous page
   */
  goBack() {
    this.navigationHistoryService.navigateToPreviousPage();
  }

}
