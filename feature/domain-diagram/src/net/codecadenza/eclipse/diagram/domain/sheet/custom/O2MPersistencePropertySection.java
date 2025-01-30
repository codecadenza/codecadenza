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

import net.codecadenza.eclipse.diagram.domain.edit.parts.OneToManyAssociationEditPart;
import net.codecadenza.eclipse.model.db.DBColumn;
import net.codecadenza.eclipse.model.db.DBTable;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import org.eclipse.gmf.runtime.notation.Edge;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CLabel;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.views.properties.tabbed.AbstractPropertySection;
import org.eclipse.ui.views.properties.tabbed.ITabbedPropertyConstants;
import org.eclipse.ui.views.properties.tabbed.TabbedPropertySheetPage;

/**
 * <p>
 * Property section for one-to-many associations
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class O2MPersistencePropertySection extends AbstractPropertySection {
	private static final String SOURCE_BEAN = "Domain object:";
	private static final String SOURCE_COLUMN = "Column:";
	private static final String SOURCE_COLUMN_TYPE = "Column type:";
	private static final String TARGET_BEAN = "Target object:";
	private static final String TARGET_COLUMN = "Target column:";
	private static final int LABEL_WIDTH = 220;

	private OneToManyAssociation association;
	private Text txtTableName;
	private Text txtSourceBeanName;
	private Text txtSourceColumn;
	private Text txtSourceColumnType;
	private Text txtTargetBeanName;
	private Text txtTargetColumn;
	private Text txtTargetColumnType;
	private CLabel lblSourceBeanName;
	private CLabel lblSourceColumn;
	private CLabel lblSourceColumnType;
	private CLabel lblTargetBeanName;
	private CLabel lblTargetColumn;
	private CLabel lblTargetColumnType;

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

		txtSourceBeanName = getWidgetFactory().createText(panPropertySection, "", SWT.READ_ONLY);
		txtSourceBeanName.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(txtSourceBeanName, -ITabbedPropertyConstants.HSPACE);
		data.top = new FormAttachment(txtSourceBeanName, 0, SWT.CENTER);

		lblSourceBeanName = getWidgetFactory().createCLabel(panPropertySection, SOURCE_BEAN);
		lblSourceBeanName.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, LABEL_WIDTH);
		data.right = new FormAttachment(0, 400);
		data.top = new FormAttachment(txtSourceBeanName, ITabbedPropertyConstants.VSPACE);

		txtSourceColumn = getWidgetFactory().createText(panPropertySection, "", SWT.READ_ONLY);
		txtSourceColumn.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(txtSourceColumn, -ITabbedPropertyConstants.HSPACE);
		data.top = new FormAttachment(txtSourceColumn, 0, SWT.CENTER);

		lblSourceColumn = getWidgetFactory().createCLabel(panPropertySection, SOURCE_COLUMN);
		lblSourceColumn.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, LABEL_WIDTH);
		data.right = new FormAttachment(0, 400);
		data.top = new FormAttachment(txtSourceBeanName, ITabbedPropertyConstants.VSPACE);

		txtSourceColumnType = getWidgetFactory().createText(panPropertySection, "", SWT.READ_ONLY);
		txtSourceColumnType.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(txtSourceColumn, -ITabbedPropertyConstants.HSPACE);
		data.top = new FormAttachment(txtSourceColumn, 0, SWT.CENTER);

		lblSourceColumnType = getWidgetFactory().createCLabel(panPropertySection, SOURCE_COLUMN_TYPE);
		lblSourceColumnType.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, LABEL_WIDTH);
		data.right = new FormAttachment(0, 400);
		data.top = new FormAttachment(txtSourceColumn, ITabbedPropertyConstants.VSPACE);

		txtTargetBeanName = getWidgetFactory().createText(panPropertySection, "", SWT.READ_ONLY);
		txtTargetBeanName.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(txtTargetBeanName, -ITabbedPropertyConstants.HSPACE);
		data.top = new FormAttachment(txtTargetBeanName, 0, SWT.CENTER);

		lblTargetBeanName = getWidgetFactory().createCLabel(panPropertySection, TARGET_BEAN);
		lblTargetBeanName.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, LABEL_WIDTH);
		data.right = new FormAttachment(0, 400);
		data.top = new FormAttachment(txtTargetBeanName, ITabbedPropertyConstants.VSPACE);

		txtTargetColumn = getWidgetFactory().createText(panPropertySection, "", SWT.READ_ONLY);
		txtTargetColumn.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(txtTargetColumn, -ITabbedPropertyConstants.HSPACE);
		data.top = new FormAttachment(txtTargetColumn, 0, SWT.CENTER);

		lblTargetColumn = getWidgetFactory().createCLabel(panPropertySection, TARGET_COLUMN);
		lblTargetColumn.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, LABEL_WIDTH);
		data.right = new FormAttachment(0, 400);
		data.top = new FormAttachment(txtTargetColumn, ITabbedPropertyConstants.VSPACE);

		txtTargetColumnType = getWidgetFactory().createText(panPropertySection, "", SWT.READ_ONLY);
		txtTargetColumnType.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(txtTargetColumnType, -ITabbedPropertyConstants.HSPACE);
		data.top = new FormAttachment(txtTargetColumnType, 0, SWT.CENTER);

		lblTargetColumnType = getWidgetFactory().createCLabel(panPropertySection, "Target column type:");
		lblTargetColumnType.setLayoutData(data);
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
		association = (OneToManyAssociation) ((Edge) ((OneToManyAssociationEditPart) input).getModel()).getElement();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.views.properties.tabbed.AbstractPropertySection#refresh()
	 */
	@Override
	public void refresh() {
		txtTableName.setText("");
		txtSourceBeanName.setText("");
		txtSourceColumn.setText("");
		txtSourceColumnType.setText("");
		txtTargetBeanName.setText("");
		txtTargetColumn.setText("");
		txtTargetColumnType.setText("");

		if (association.isBidirectional()) {
			// There exists no table, but a column in the target bean, show these properties
			DBColumn column = null;
			final ManyToOneAssociation mto = association.getReverseAssociation();

			if (mto != null)
				column = mto.getColumn();
			else
				return;

			txtTableName.setText(column.getDatabaseTable().getName());

			lblSourceBeanName.setText("Column name:");
			txtSourceBeanName.setText(column.getName());

			lblSourceColumn.setText(SOURCE_COLUMN_TYPE);
			txtSourceColumn.setText(column.getColumnType().getName());

			lblSourceColumnType.setText("Length:");
			txtSourceColumnType.setText("" + column.getLength());

			lblTargetBeanName.setText("Precision:");
			txtTargetBeanName.setText("" + column.getPrecision());

			lblTargetColumn.setText("Scale:");
			txtTargetColumn.setText("" + column.getScale());

			lblTargetColumnType.setVisible(false);
			txtTargetColumnType.setVisible(false);
		}
		else {
			final DBTable table = association.getTable();

			if (table == null)
				return;

			lblTargetColumnType.setVisible(true);
			txtTargetColumnType.setVisible(true);

			lblSourceBeanName.setText(SOURCE_BEAN);
			lblSourceColumn.setText(SOURCE_COLUMN);
			lblSourceColumnType.setText(SOURCE_COLUMN_TYPE);
			lblTargetBeanName.setText(TARGET_BEAN);
			lblTargetColumn.setText(TARGET_COLUMN);

			txtTableName.setText(table.getName());
			txtSourceBeanName.setText(association.getDomainObject().getName());
			txtSourceColumn.setText(table.getColumns().toArray(new DBColumn[association.getTable().getColumns().size()])[0].getName());
			txtSourceColumnType.setText(
					table.getColumns().toArray(new DBColumn[association.getTable().getColumns().size()])[0].getColumnType().getName());
			txtTargetBeanName.setText(association.getTarget().getName());
			txtTargetColumn.setText(table.getColumns().toArray(new DBColumn[association.getTable().getColumns().size()])[1].getName());
			txtTargetColumnType.setText(
					table.getColumns().toArray(new DBColumn[association.getTable().getColumns().size()])[1].getColumnType().getName());
		}
	}

}
