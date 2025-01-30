import { AbstractSingleRecordForm } from './abstract-single-record-form';
import { Observable, of } from 'rxjs';

/**
 * Base class for all single-record forms that display data of an existing object
 */
// eslint-disable-next-line @typescript-eslint/no-explicit-any
export abstract class AbstractReadRecordForm<T extends { [key: string]: any; }> extends AbstractSingleRecordForm<T> {

  /**
   * This kind of form doesn't save an object
   */
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  saveObject(_object: T): Observable<T> {
    return of();
  }

}
