import { AbstractSingleRecordForm } from './abstract-single-record-form';
import { UntypedFormGroup } from '@angular/forms';
import { Observable, of } from 'rxjs';

/**
 * Base class for all single-record forms that add a new object to a parent object
 */
// eslint-disable-next-line @typescript-eslint/no-explicit-any
export abstract class AbstractAddRecordForm<T extends { [key: string]: any; }> extends AbstractSingleRecordForm<T> {
  parentObjectId!: string;

  /**
   * Initialize this form
   */
  override initForm() {
    this.object = {} as T;
    const id = this.route.snapshot.paramMap.get('id');

    if (!id) {
      this.openErrorDialog(new Error(this.i18n.translate('msg_missingidparameter')), true);
      return;
    }

    this.parentObjectId = id;

    console.log('Open page with parent object ID ' + this.parentObjectId);

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
