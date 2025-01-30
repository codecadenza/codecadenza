import { FieldTypeEnum } from './field-type.enum';

/*
 * Class that simplifies the setup of a table component
 */
export class TableDefinition {
  private readonly columns: ColumnDefinition[] = [];

  /**
   * Create a new column definition by using the provided parameters and add it to the table
   */
  addColumn(fieldName: string, header: string, fieldType: FieldTypeEnum, width: number): ColumnDefinition {
    const column: ColumnDefinition = {
      field: fieldName,
      header: header,
      type: fieldType,
      width: width + 'px',
      dateTimeFormat: false
    };

    if (column.type === FieldTypeEnum.DATE) {
      column.dateTimeFormat = true;
    }

    this.columns.push(column);

    return column;
  }

  /**
   * Search a column definition by its field name. The method will return 'undefined' if the column could not be found!
   */
  getColumnByFieldName(fieldName: string): ColumnDefinition | undefined {
    return this.columns.find(col => col.field === fieldName);
  }

  /**
   * Return an array that contains all table columns
   */
  getColumns(): Array<ColumnDefinition> {
    return this.columns;
  }
}

/**
 * Interface column-related data
 */
export interface ColumnDefinition {
  field: string;
  header: string;
  type: FieldTypeEnum;
  dateTimeFormat: boolean;
  width: string;
}
