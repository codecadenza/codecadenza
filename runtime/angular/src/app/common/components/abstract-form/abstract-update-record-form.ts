import { AbstractSingleRecordForm } from './abstract-single-record-form';

/**
 * Base class for all single-record forms that update an existing object
 */
// eslint-disable-next-line @typescript-eslint/no-explicit-any
export abstract class AbstractUpdateRecordForm<T extends { [key: string]: any; }> extends AbstractSingleRecordForm<T> {}
