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
package net.codecadenza.eclipse.diagram.domain.sheet.custom;

import net.codecadenza.eclipse.diagram.domain.edit.parts.DomainAttributeEditPart;
import net.codecadenza.eclipse.model.db.DBColumn;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import org.eclipse.gmf.runtime.notation.Node;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CLabel;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.views.properties.tabbed.AbstractPropertySection;
import org.eclipse.ui.views.properties.tabbed.ITabbedPropertyConstants;
import org.eclipse.ui.views.properties.tabbed.TabbedPropertySheetPage;

/**
 * <p>
 * Property section for database columns
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DbColumnPropertySection extends AbstractPropertySection {
	private static final int LABEL_WIDTH = 220;
	private DBColumn column;
	private Text txtTableName;
	private Text txtColumnName;
	private Text txtColumnType;
	private Text txtLength;
	private Text txtPrecision;
	private Text txtScale;
	private Button chkNullable;

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.views.properties.tabbed.AbstractPropertySection#createControls(org.eclipse.swt.widgets.Composite,
	 * org.eclipse.ui.views.properties.tabbed.TabbedPropertySheetPage)
	 */
	@Override
	public void createControls(Composite parent, TabbedPropertySheetPage tabbedPropertySheetPage) {
		super.createControls(parent, tabbedPropertySheetPage);

		final Composite panPropertySection = getWidgetFactory().createFlatFormComposite(parent);

		var data = new FormData();
		data.left = new FormAttachment(0, LABEL_WIDTH);
		data.right = new FormAttachment(0, 400);
		data.top = new FormAttachment(0, ITabbedPropertyConstants.VSPACE);

		txtTableName = getWidgetFactory().createText(panPropertySection, "", SWT.READ_ONLY);
		txtTableName.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(txtTableName, -ITabbedPropertyConstants.HSPACE);
		data.top = new FormAttachment(txtTableName, 0, SWT.CENTER);

		final CLabel lblTableName = getWidgetFactory().createCLabel(panPropertySection, "Table name:");
		lblTableName.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, LABEL_WIDTH);
		data.right = new FormAttachment(0, 400);
		data.top = new FormAttachment(txtTableName, ITabbedPropertyConstants.VSPACE);

		txtColumnName = getWidgetFactory().createText(panPropertySection, "", SWT.READ_ONLY);
		txtColumnName.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(txtColumnName, -ITabbedPropertyConstants.HSPACE);
		data.top = new FormAttachment(txtColumnName, 0, SWT.CENTER);

		final CLabel lblColumnName = getWidgetFactory().createCLabel(panPropertySection, "Column name:");
		lblColumnName.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, LABEL_WIDTH);
		data.right = new FormAttachment(0, 400);
		data.top = new FormAttachment(txtColumnName, ITabbedPropertyConstants.VSPACE);

		txtColumnType = getWidgetFactory().createText(panPropertySection, "", SWT.READ_ONLY);
		txtColumnType.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(txtColumnType, -ITabbedPropertyConstants.HSPACE);
		data.top = new FormAttachment(txtColumnType, 0, SWT.CENTER);

		final CLabel lblColumnType = getWidgetFactory().createCLabel(panPropertySection, "Column type:");
		lblColumnType.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, LABEL_WIDTH);
		data.right = new FormAttachment(0, 400);
		data.top = new FormAttachment(txtColumnType, ITabbedPropertyConstants.VSPACE);

		txtLength = getWidgetFactory().createText(panPropertySection, "", SWT.READ_ONLY);
		txtLength.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(txtLength, -ITabbedPropertyConstants.HSPACE);
		data.top = new FormAttachment(txtLength, 0, SWT.CENTER);

		final CLabel lblLength = getWidgetFactory().createCLabel(panPropertySection, "Length:");
		lblLength.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, LABEL_WIDTH);
		data.right = new FormAttachment(0, 400);
		data.top = new FormAttachment(txtLength, ITabbedPropertyConstants.VSPACE);

		txtPrecision = getWidgetFactory().createText(panPropertySection, "", SWT.READ_ONLY);
		txtPrecision.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(txtPrecision, -ITabbedPropertyConstants.HSPACE);
		data.top = new FormAttachment(txtPrecision, 0, SWT.CENTER);

		final CLabel lblPrecision = getWidgetFactory().createCLabel(panPropertySection, "Precision:");
		lblPrecision.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, LABEL_WIDTH);
		data.right = new FormAttachment(0, 400);
		data.top = new FormAttachment(txtPrecision, ITabbedPropertyConstants.VSPACE);

		txtScale = getWidgetFactory().createText(panPropertySection, "", SWT.READ_ONLY);
		txtScale.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(txtScale, -ITabbedPropertyConstants.HSPACE);
		data.top = new FormAttachment(txtScale, 0, SWT.CENTER);

		final CLabel lblScale = getWidgetFactory().createCLabel(panPropertySection, "Scale:");
		lblScale.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, LABEL_WIDTH);
		data.right = new FormAttachment(100, 0);
		data.top = new FormAttachment(txtScale, ITabbedPropertyConstants.VSPACE);

		chkNullable = getWidgetFactory().createButton(panPropertySection, "", SWT.CHECK);
		chkNullable.setLayoutData(data);
		chkNullable.setEnabled(false);

		data = new FormData();
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(chkNullable, -ITabbedPropertyConstants.HSPACE);
		data.top = new FormAttachment(chkNullable, 0, SWT.CENTER);

		final CLabel lblNullable = getWidgetFactory().createCLabel(panPropertySection, "Nullable:");
		lblNullable.setLayoutData(data);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.views.properties.tabbed.AbstractPropertySection#refresh()
	 */
	@Override
	public void refresh() {
		if (column == null)
			return;

		txtTableName.setText(column.getDatabaseTable().getName());
		txtColumnName.setText(column.getName());
		txtColumnType.setText(column.getColumnType().getName());
		txtLength.setText(Integer.toString(column.getLength()));
		txtPrecision.setText(Integer.toString(column.getPrecision()));
		txtScale.setText(Integer.toString(column.getScale()));
		chkNullable.setSelection(column.isNullable());
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.views.properties.tabbed.AbstractPropertySection#setInput(org.eclipse.ui.IWorkbenchPart,
	 * org.eclipse.jface.viewers.ISelection)
	 */
	@Override
	public void setInput(IWorkbenchPart part, ISelection selection) {
		super.setInput(part, selection);

		final Object input = ((IStructuredSelection) selection).getFirstElement();
		column = ((DomainAttribute) ((Node) ((DomainAttributeEditPart) input).getModel()).getElement()).getColumn();
	}

}
