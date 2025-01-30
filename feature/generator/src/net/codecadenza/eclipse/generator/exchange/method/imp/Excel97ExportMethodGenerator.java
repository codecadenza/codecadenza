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
 * Generator for methods that export data to Microsoft Excel 97 files
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class Excel97ExportMethodGenerator extends AbstractExcelExportMethodGenerator {
	/**
	 * Constructor
	 * @param generator
	 * @param method
	 */
	public Excel97ExportMethodGenerator(AbstractJavaSourceGenerator generator, DataExchangeMethod method) {
		super(generator, method);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.imp.AbstractExcelExportMethodGenerator#getWorkbookClassName()
	 */
	@Override
	protected String getWorkbookClassName() {
		return "HSSFWorkbook";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.imp.AbstractExcelExportMethodGenerator#getRowClassName()
	 */
	@Override
	protected String getRowClassName() {
		return "HSSFRow";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.imp.AbstractExcelExportMethodGenerator#getSheetClassName()
	 */
	@Override
	protected String getSheetClassName() {
		return "HSSFSheet";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.imp.AbstractExcelExportMethodGenerator#getCellClassName()
	 */
	@Override
	protected String getCellClassName() {
		return "HSSFCell";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.imp.AbstractExcelExportMethodGenerator#getFontClassName()
	 */
	@Override
	protected String getFontClassName() {
		return "HSSFFont";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.imp.AbstractExcelExportMethodGenerator#getCellStyleClassName()
	 */
	@Override
	protected String getCellStyleClassName() {
		return "HSSFCellStyle";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.imp.AbstractExcelExportMethodGenerator#getSpreadsheetVersion()
	 */
	@Override
	protected String getSpreadsheetVersion() {
		return "EXCEL97";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.imp.AbstractExcelExportMethodGenerator#
	 * getDataValidationHelperClassName()
	 */
	@Override
	protected String getDataValidationHelperClassName() {
		return "HSSFDataValidationHelper";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.imp.AbstractExcelExportMethodGenerator#
	 * getDataValidationConstraintClassName()
	 */
	@Override
	protected String getDataValidationConstraintClassName() {
		return "DVConstraint";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.imp.AbstractExcelExportMethodGenerator#getHeaderColorFragment(String)
	 */
	@Override
	protected String getHeaderColorFragment(String cellStyleObjectName) {
		return cellStyleObjectName + ".setFillForegroundColor(HSSFColor.HSSFColorPredefined.GREY_25_PERCENT.getIndex());\n";
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

		generator.importPackage("org.apache.poi.hssf.usermodel");
		generator.importPackage("org.apache.poi.hssf.util");
	}

}
