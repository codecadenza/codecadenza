/*
 * This file is part of CodeCadenza, a set of tools, libraries and plug-ins
 * for modeling and creating Java-based enterprise applications.
 * For more information visit:
 *
 * https://github.com/codecadenza/
 *
 * This software is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
 */
package net.codecadenza.eclipse.generator.exchange.method.imp;

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;

/**
 * <p>
 * Generator for methods that export data to Microsoft Excel 2007 files
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class Excel2007ExportMethodGenerator extends AbstractExcelExportMethodGenerator {
	/**
	 * Constructor
	 * @param generator
	 * @param method
	 */
	public Excel2007ExportMethodGenerator(AbstractJavaSourceGenerator generator, DataExchangeMethod method) {
		super(generator, method);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.imp.AbstractExcelExportMethodGenerator#getWorkbookClassName()
	 */
	@Override
	protected String getWorkbookClassName() {
		return "XSSFWorkbook";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.imp.AbstractExcelExportMethodGenerator#getRowClassName()
	 */
	@Override
	protected String getRowClassName() {
		return "XSSFRow";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.imp.AbstractExcelExportMethodGenerator#getSheetClassName()
	 */
	@Override
	protected String getSheetClassName() {
		return "XSSFSheet";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.imp.AbstractExcelExportMethodGenerator#getCellClassName()
	 */
	@Override
	protected String getCellClassName() {
		return "XSSFCell";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.imp.AbstractExcelExportMethodGenerator#getFontClassName()
	 */
	@Override
	protected String getFontClassName() {
		return "XSSFFont";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.imp.AbstractExcelExportMethodGenerator#getCellStyleClassName()
	 */
	@Override
	protected String getCellStyleClassName() {
		return "XSSFCellStyle";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.imp.AbstractExcelExportMethodGenerator#getSpreadsheetVersion()
	 */
	@Override
	protected String getSpreadsheetVersion() {
		return "EXCEL2007";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.imp.AbstractExcelExportMethodGenerator#
	 * getDataValidationHelperClassName()
	 */
	@Override
	protected String getDataValidationHelperClassName() {
		return "XSSFDataValidationHelper";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.imp.AbstractExcelExportMethodGenerator#
	 * getDataValidationConstraintClassName()
	 */
	@Override
	protected String getDataValidationConstraintClassName() {
		return "XSSFDataValidationConstraint";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.imp.AbstractExcelExportMethodGenerator#suppressDropDownArrow()
	 */
	@Override
	protected boolean suppressDropDownArrow() {
		// Even if we don't want to suppress the drop down arrow we must return true in order to get the expected behaviour!
		return true;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.imp.AbstractExcelExportMethodGenerator#addImports()
	 */
	@Override
	public void addImports() {
		super.addImports();

		if (generator == null)
			return;

		generator.importPackage("org.apache.poi.xssf.usermodel");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.imp.AbstractExcelExportMethodGenerator#getHeaderColorFragment(String)
	 */
	@Override
	protected String getHeaderColorFragment(String cellStyleObjectName) {
		return cellStyleObjectName + ".setFillForegroundColor(new XSSFColor(new byte[]{(byte)0xE0,(byte)0xE0,(byte)0xE0}, null));\n";
	}

}
