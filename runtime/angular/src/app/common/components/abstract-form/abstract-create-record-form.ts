import { AbstractSingleRecordForm } from './abstract-single-record-form';
import { UntypedFormGroup } from '@angular/forms';
import { Observable, of } from 'rxjs';

/**
 * Base class for all single-record forms that create a new object
 */
// eslint-disable-next-line @typescript-eslint/no-explicit-any
export abstract class AbstractCreateRecordForm<T extends { [key: string]: any; }> extends AbstractSingleRecordForm<T> {

  /**
   * Initialize the form
   */
  override initForm() {
    this.object = {} as T;

    // Create the main form group
    this.formGroup = new UntypedFormGroup({});

    // Add controls
    this.addControls();
  }

  /**
   * This kind of form doesn't load an object
   */
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  loadObject(_id: string): Observable<T> {
    return of();
  }

}
