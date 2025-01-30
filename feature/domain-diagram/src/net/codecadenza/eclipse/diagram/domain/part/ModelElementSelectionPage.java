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
package net.codecadenza.eclipse.diagram.domain.part;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.util.FeatureMap;
import org.eclipse.emf.edit.provider.IWrapperItemProvider;
import org.eclipse.emf.edit.ui.provider.AdapterFactoryContentProvider;
import org.eclipse.emf.edit.ui.provider.AdapterFactoryLabelProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;

/**
 * <p>
 * Selection page for a model element
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ModelElementSelectionPage extends WizardPage {
	protected EObject selectedModelElement;
	private TreeViewer modelViewer;

	/**
	 * @param pageName
	 */
	public ModelElementSelectionPage(String pageName) {
		super(pageName);
	}

	/**
	 * @return the model element
	 */
	public EObject getModelElement() {
		return selectedModelElement;
	}

	/**
	 * @param modelElement
	 */
	public void setModelElement(EObject modelElement) {
		selectedModelElement = modelElement;

		if (modelViewer != null) {
			if (selectedModelElement != null) {
				modelViewer.setInput(selectedModelElement.eResource());
				modelViewer.setSelection(new StructuredSelection(selectedModelElement));
			}
			else
				modelViewer.setInput(null);

			setPageComplete(validatePage());
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	public void createControl(Composite parent) {
		initializeDialogUnits(parent);

		final var glPageArea = new GridLayout();
		glPageArea.marginWidth = 0;

		final var panPageArea = new Composite(parent, SWT.NONE);
		panPageArea.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		panPageArea.setLayout(glPageArea);

		setControl(panPageArea);

		final var lblSelection = new Label(panPageArea, SWT.NONE);
		lblSelection.setText(getSelectionTitle());
		lblSelection.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING));

		final var glModelViewer = new GridData(SWT.FILL, SWT.FILL, true, true);
		glModelViewer.heightHint = 300;
		glModelViewer.widthHint = 300;

		modelViewer = new TreeViewer(panPageArea, SWT.SINGLE | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER);
		modelViewer.getTree().setLayoutData(glModelViewer);
		modelViewer.setContentProvider(
				new AdapterFactoryContentProvider(CodeCadenzaDiagramEditorPlugin.getInstance().getItemProvidersAdapterFactory()));
		modelViewer.setLabelProvider(
				new AdapterFactoryLabelProvider(CodeCadenzaDiagramEditorPlugin.getInstance().getItemProvidersAdapterFactory()));

		if (selectedModelElement != null) {
			modelViewer.setInput(selectedModelElement.eResource());
			modelViewer.setSelection(new StructuredSelection(selectedModelElement));
		}

		modelViewer.addSelectionChangedListener(
				event -> ModelElementSelectionPage.this.updateSelection((IStructuredSelection) event.getSelection()));

		setPageComplete(validatePage());
	}

	/**
	 * Get a custom model element description
	 * @return the selection title
	 */
	protected String getSelectionTitle() {
		return Messages.ModelElementSelectionPageMessage;
	}

	/**
	 * @param selection
	 */
	protected void updateSelection(IStructuredSelection selection) {
		selectedModelElement = null;

		if (selection.size() == 1) {
			Object selectedElement = selection.getFirstElement();

			if (selectedElement instanceof final IWrapperItemProvider wrapperItemProvider)
				selectedElement = wrapperItemProvider.getValue();

			if (selectedElement instanceof final FeatureMap.Entry entry)
				selectedElement = entry.getValue();

			if (selectedElement instanceof final EObject eObject)
				selectedModelElement = eObject;
		}

		setPageComplete(validatePage());
	}

	/**
	 * Validate the wizard page
	 * @return always true
	 */
	protected boolean validatePage() {
		return true;
	}

}
