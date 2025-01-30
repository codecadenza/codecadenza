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

import static net.codecadenza.eclipse.shared.Constants.UTF_8;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.service.domain.DomainObjectService;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.xmi.XMLResource;
import org.eclipse.gef.EditPart;
import org.eclipse.gmf.runtime.diagram.ui.editparts.DiagramEditPart;
import org.eclipse.gmf.runtime.diagram.ui.editparts.IGraphicalEditPart;
import org.eclipse.gmf.runtime.diagram.ui.editparts.IPrimaryEditPart;
import org.eclipse.gmf.runtime.diagram.ui.parts.IDiagramGraphicalViewer;
import org.eclipse.gmf.runtime.diagram.ui.parts.IDiagramWorkbenchPart;
import org.eclipse.gmf.runtime.emf.core.util.EMFCoreUtil;
import org.eclipse.gmf.runtime.notation.View;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.FileEditorInput;

/**
 * <p>
 * Diagram editor utility class
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CodeCadenzaDiagramEditorUtil {
	/**
	 * Prevent instantiation
	 */
	private CodeCadenzaDiagramEditorUtil() {

	}

	/**
	 * @return a map containing the save options
	 */
	public static Map<String, String> getSaveOptions() {
		final var saveOptions = new HashMap<String, String>();
		saveOptions.put(XMLResource.OPTION_ENCODING, UTF_8);
		saveOptions.put(Resource.OPTION_SAVE_ONLY_IF_CHANGED, Resource.OPTION_SAVE_ONLY_IF_CHANGED_MEMORY_BUFFER);

		return saveOptions;
	}

	/**
	 * @param diagram
	 * @return true if the diagram can be opened
	 * @throws PartInitException if the diagram could not be opened
	 */
	public static boolean openDiagram(Resource diagram) throws PartInitException {
		final String path = diagram.getURI().toPlatformString(true);
		final IResource workspaceResource = ResourcesPlugin.getWorkspace().getRoot().findMember(new Path(path));

		if (workspaceResource instanceof final IFile file) {
			final IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
			return null != page.openEditor(new FileEditorInput(file), CodeCadenzaDiagramEditor.ID);
		}

		return false;
	}

	/**
	 * @param file
	 */
	public static void setCharset(IFile file) {
		if (file == null)
			return;

		try {
			file.setCharset(UTF_8, new NullProgressMonitor());
		}
		catch (final CoreException e) {
			CodeCadenzaDiagramEditorPlugin.getInstance().logError("Unable to set charset for file " + file.getFullPath(), e);
		}
	}

	/**
	 * @param containerFullPath
	 * @param fileName
	 * @param extension
	 * @return the unique file name
	 */
	public static String getUniqueFileName(IPath containerFullPath, String fileName, String extension) {
		if (containerFullPath == null)
			containerFullPath = new Path("");

		if (fileName == null || fileName.trim().isEmpty())
			fileName = "default";

		IPath filePath = containerFullPath.append(fileName);

		if (extension != null && !extension.equals(filePath.getFileExtension()))
			filePath = filePath.addFileExtension(extension);

		extension = filePath.getFileExtension();
		fileName = filePath.removeFileExtension().lastSegment();
		int i = 1;

		while (ResourcesPlugin.getWorkspace().getRoot().exists(filePath)) {
			i++;
			filePath = containerFullPath.append(fileName + i);

			if (extension != null)
				filePath = filePath.addFileExtension(extension);
		}

		return filePath.lastSegment();
	}

	/**
	 * Run the wizard in a dialog
	 * @param shell
	 * @param wizard
	 * @param settingsKey
	 */
	public static void runWizard(Shell shell, Wizard wizard, String settingsKey) {
		final IDialogSettings pluginDialogSettings = CodeCadenzaDiagramEditorPlugin.getInstance().getDialogSettings();
		IDialogSettings wizardDialogSettings = pluginDialogSettings.getSection(settingsKey);

		if (wizardDialogSettings == null)
			wizardDialogSettings = pluginDialogSettings.addNewSection(settingsKey);

		wizard.setDialogSettings(wizardDialogSettings);

		final var dialog = new WizardDialog(shell, wizard);
		dialog.create();
		dialog.getShell().setSize(Math.max(500, dialog.getShell().getSize().x), 500);
		dialog.open();
	}

	/**
	 * @param diagramPart
	 * @param editParts
	 */
	public static void selectElementsInDiagram(IDiagramWorkbenchPart diagramPart, List<EditPart> editParts) {
		diagramPart.getDiagramGraphicalViewer().deselectAll();

		EditPart firstPrimary = null;

		for (final EditPart part : editParts) {
			diagramPart.getDiagramGraphicalViewer().appendSelection(part);

			if (firstPrimary == null && part instanceof IPrimaryEditPart)
				firstPrimary = part;
		}

		if (!editParts.isEmpty())
			diagramPart.getDiagramGraphicalViewer().reveal(firstPrimary != null ? firstPrimary : editParts.get(0));
	}

	/**
	 * @param diagramPart
	 * @param element
	 * @param editPartCollector
	 * @return the number of edit parts
	 */
	private static int findElementsInDiagramByID(DiagramEditPart diagramPart, EObject element, List<EditPart> editPartCollector) {
		final var viewer = (IDiagramGraphicalViewer) diagramPart.getViewer();
		final int intialNumOfEditParts = editPartCollector.size();

		if (element instanceof View) {
			// Lookup for the edit part
			final EditPart editPart = viewer.getEditPartRegistry().get(element);

			if (editPart != null) {
				editPartCollector.add(editPart);
				return 1;
			}
		}

		final String elementID = EMFCoreUtil.getProxyID(element);
		final List<EditPart> associatedParts = viewer.findEditPartsForElement(elementID, IGraphicalEditPart.class);

		// Perform the possible hierarchy disjoint -> take the top-most parts only
		associatedParts.forEach(part -> {
			EditPart parentPart = part.getParent();

			while (parentPart != null && !associatedParts.contains(parentPart))
				parentPart = parentPart.getParent();

			if (parentPart == null)
				editPartCollector.add(part);
		});

		if (intialNumOfEditParts == editPartCollector.size())
			if (!associatedParts.isEmpty())
				editPartCollector.add(associatedParts.iterator().next());
			else if (element.eContainer() != null)
				return findElementsInDiagramByID(diagramPart, element.eContainer(), editPartCollector);

		return editPartCollector.size() - intialNumOfEditParts;
	}

	/**
	 * @param diagramEditPart
	 * @param targetElement
	 * @param lazyElement2ViewMap
	 * @return the view
	 */
	public static View findView(DiagramEditPart diagramEditPart, EObject targetElement, LazyElement2ViewMap lazyElement2ViewMap) {
		boolean hasStructuralURI = false;

		if (targetElement.eResource() instanceof final XMLResource xmlResource)
			hasStructuralURI = xmlResource.getID(targetElement) == null;

		View view = null;

		if (hasStructuralURI && !lazyElement2ViewMap.getElement2ViewMap().isEmpty())
			view = (View) lazyElement2ViewMap.getElement2ViewMap().get(targetElement);
		else if (findElementsInDiagramByID(diagramEditPart, targetElement, lazyElement2ViewMap.editPartTmpHolder) > 0) {
			final EditPart editPart = lazyElement2ViewMap.editPartTmpHolder.get(0);
			lazyElement2ViewMap.editPartTmpHolder.clear();
			view = editPart.getModel() instanceof final View model ? model : null;
		}

		return (view == null) ? diagramEditPart.getDiagramView() : view;
	}

	public static class LazyElement2ViewMap {
		private Map<EObject, EObject> element2ViewMap;
		private final View scope;
		private final Set<EObject> elementSet;
		public final List<EditPart> editPartTmpHolder = new ArrayList<>();

		/**
		 * @param scope
		 * @param elements
		 */
		public LazyElement2ViewMap(View scope, Set<EObject> elements) {
			this.scope = scope;
			this.elementSet = elements;
		}

		/**
		 * @return the view map
		 */
		public final Map<EObject, EObject> getElement2ViewMap() {
			if (element2ViewMap == null) {
				element2ViewMap = new HashMap<>();

				// Map possible notation elements to itself as these can't be found by view.getElement()
				elementSet.stream().filter(View.class::isInstance).forEach(element -> {
					final var view = (View) element;

					if (view.getDiagram() == scope.getDiagram()) {
						// Take only those that are part of our diagram
						element2ViewMap.put(element, element);
					}
				});

				buildElement2ViewMap(scope, element2ViewMap, elementSet);
			}

			return element2ViewMap;
		}

		/**
		 * @param parentView
		 * @param element2ViewMap
		 * @param elements
		 * @return the map
		 */
		static Map<EObject, EObject> buildElement2ViewMap(View parentView, Map<EObject, EObject> element2ViewMap,
				Set<EObject> elements) {
			if (elements.size() == element2ViewMap.size())
				return element2ViewMap;

			if (parentView.isSetElement() && !element2ViewMap.containsKey(parentView.getElement())
					&& elements.contains(parentView.getElement())) {
				element2ViewMap.put(parentView.getElement(), parentView);

				if (elements.size() == element2ViewMap.size())
					return element2ViewMap;
			}

			for (final Iterator<View> it = parentView.getChildren().iterator(); it.hasNext();) {
				buildElement2ViewMap(it.next(), element2ViewMap, elements);

				if (elements.size() == element2ViewMap.size())
					return element2ViewMap;
			}

			for (final Iterator<View> it = parentView.getSourceEdges().iterator(); it.hasNext();) {
				buildElement2ViewMap(it.next(), element2ViewMap, elements);

				if (elements.size() == element2ViewMap.size())
					return element2ViewMap;
			}

			for (final Iterator<View> it = parentView.getSourceEdges().iterator(); it.hasNext();) {
				buildElement2ViewMap(it.next(), element2ViewMap, elements);

				if (elements.size() == element2ViewMap.size())
					return element2ViewMap;
			}

			return element2ViewMap;
		}
	}

	/**
	 * Rebuild all domain objects
	 * @param project
	 * @param includingSuperclasses
	 * @param includingAbstractClasses
	 */
	public static void rebuildDomainObjects(Project project, boolean includingSuperclasses, boolean includingAbstractClasses) {
		final var job = new Job("Rebuilding domain objects...") {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)
			 */
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				try {
					final var domainObjectService = new DomainObjectService(project);

					for (final DomainObject domainObject : project.getAllDomainObjectsOfProject(includingSuperclasses,
							includingAbstractClasses)) {
						monitor.beginTask("Rebuild domain object " + domainObject.getName(), IProgressMonitor.UNKNOWN);

						domainObjectService.rebuildDomainObjectSourceFiles(domainObject, false);
					}
				}
				catch (final Exception e) {
					Display.getDefault().syncExec(() -> CodeCadenzaDiagramEditorPlugin.getInstance().handleInternalError(e));
				}

				monitor.done();
				return Status.OK_STATUS;
			}
		};

		job.schedule();
	}

}
