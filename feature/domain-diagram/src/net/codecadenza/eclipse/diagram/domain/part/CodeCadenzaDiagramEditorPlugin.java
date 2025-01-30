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

import java.util.ArrayList;
import java.util.List;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.emf.common.notify.AdapterFactory;
import org.eclipse.emf.edit.provider.ComposedAdapterFactory;
import org.eclipse.emf.edit.provider.IItemLabelProvider;
import org.eclipse.emf.edit.provider.ReflectiveItemProviderAdapterFactory;
import org.eclipse.emf.edit.provider.resource.ResourceItemProviderAdapterFactory;
import org.eclipse.emf.edit.ui.provider.ExtendedImageRegistry;
import org.eclipse.gmf.runtime.diagram.core.preferences.PreferencesHint;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/**
 * <p>
 * Diagram editor plug-in
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CodeCadenzaDiagramEditorPlugin extends AbstractUIPlugin {
	public static final String ID = "net.codecadenza.eclipse.diagram";
	public static final PreferencesHint DIAGRAM_PREFERENCES_HINT = new PreferencesHint(ID);
	private static CodeCadenzaDiagramEditorPlugin instance;
	private ComposedAdapterFactory adapterFactory;
	private CodeCadenzaDocumentProvider documentProvider;

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext)
	 */
	@Override
	public synchronized void start(BundleContext context) throws Exception {
		super.start(context);

		instance = this;
		PreferencesHint.registerPreferenceStore(DIAGRAM_PREFERENCES_HINT, getPreferenceStore());
		adapterFactory = createAdapterFactory();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
	 */
	@Override
	public synchronized void stop(BundleContext context) throws Exception {
		adapterFactory.dispose();
		adapterFactory = null;
		instance = null;

		super.stop(context);
	}

	/**
	 * @return the shared instance
	 */
	public static CodeCadenzaDiagramEditorPlugin getInstance() {
		return instance;
	}

	/**
	 * @return the adapter factory
	 */
	protected ComposedAdapterFactory createAdapterFactory() {
		final var factories = new ArrayList<AdapterFactory>();
		fillItemProviderFactories(factories);

		return new ComposedAdapterFactory(factories);
	}

	/**
	 * @param factories
	 */
	protected void fillItemProviderFactories(List<AdapterFactory> factories) {
		factories.add(new ResourceItemProviderAdapterFactory());
		factories.add(new ReflectiveItemProviderAdapterFactory());
	}

	/**
	 * @return the adapter factory
	 */
	public AdapterFactory getItemProvidersAdapterFactory() {
		return adapterFactory;
	}

	/**
	 * @param item
	 * @return the image descriptor
	 */
	public ImageDescriptor getItemImageDescriptor(Object item) {
		final var labelProvider = (IItemLabelProvider) adapterFactory.adapt(item, IItemLabelProvider.class);

		if (labelProvider != null)
			return ExtendedImageRegistry.getInstance().getImageDescriptor(labelProvider.getImage(item));

		return null;
	}

	/**
	 * Get an image descriptor for the image file at the given plug-in relative path
	 * @param path
	 * @return the image descriptor
	 */
	public static ImageDescriptor getBundledImageDescriptor(String path) {
		return AbstractUIPlugin.imageDescriptorFromPlugin(ID, path);
	}

	/**
	 * Get an image descriptor for the image file in any plug-in. If the path is relative, then this bundle is looked up for the
	 * image, otherwise, for an absolute path, the first segment is taken as the ID of the plug-in with the image
	 * @param path the path to image, either absolute (with plug-in id as first segment), or relative for bundled images
	 * @return the image descriptor
	 */
	public static ImageDescriptor findImageDescriptor(String path) {
		final var p = new Path(path);

		if (p.isAbsolute() && p.segmentCount() > 1)
			return AbstractUIPlugin.imageDescriptorFromPlugin(p.segment(0), p.removeFirstSegments(1).makeAbsolute().toString());

		return getBundledImageDescriptor(p.makeAbsolute().toString());
	}

	/**
	 * Get an image for the image file at the given plug-in relative path. A client doesn't need to dispose this image as it will be
	 * disposed automatically!
	 * @param path the path
	 * @return the image instance
	 */
	public Image getBundledImage(String path) {
		Image image = getImageRegistry().get(path);

		if (image == null) {
			getImageRegistry().put(path, getBundledImageDescriptor(path));
			image = getImageRegistry().get(path);
		}

		return image;
	}

	/**
	 * @param key
	 * @return the string from the plug-in's resource bundle
	 */
	public static String getString(String key) {
		return Platform.getResourceString(getInstance().getBundle(), "%" + key);
	}

	/**
	 * @return the document provider
	 */
	public CodeCadenzaDocumentProvider getDocumentProvider() {
		if (documentProvider == null)
			documentProvider = new CodeCadenzaDocumentProvider();

		return documentProvider;
	}

	/**
	 * Log the error and open a dialog in order to inform the user that an error has occurred
	 * @param t
	 */
	public void handleInternalError(Throwable t) {
		final Shell shell = Display.getCurrent().getActiveShell();

		// An IllegalStateException usually indicates that an internal validation error has occurred!
		if (t instanceof IllegalStateException) {
			MessageDialog.openWarning(shell, "CodeCadenza - Validation Error", t.getMessage());
			return;
		}

		MessageDialog.openError(shell, "CodeCadenza - Internal Error", "An internal error has occurred! Message: " + t.getMessage());
		logError(t);
	}

	/**
	 * @param throwable
	 */
	public void logError(Throwable throwable) {
		logError(null, throwable);
	}

	/**
	 * @param error
	 * @param throwable
	 */
	public void logError(String error, Throwable throwable) {
		if (error == null && throwable != null)
			error = throwable.getMessage();

		getLog().log(new Status(IStatus.ERROR, CodeCadenzaDiagramEditorPlugin.ID, IStatus.OK, error, throwable));
		debug(error, throwable);
	}

	/**
	 * @param message
	 */
	public void logInfo(String message) {
		logInfo(message, null);
	}

	/**
	 * @param message
	 * @param throwable
	 */
	public void logInfo(String message, Throwable throwable) {
		if (message == null && throwable != null)
			message = throwable.getMessage();

		getLog().log(new Status(IStatus.INFO, CodeCadenzaDiagramEditorPlugin.ID, IStatus.OK, message, throwable));
		debug(message, throwable);
	}

	/**
	 * @param message
	 * @param throwable
	 */
	private void debug(String message, Throwable throwable) {
		if (!isDebugging())
			return;

		if (message != null)
			System.err.println(message);

		if (throwable != null)
			throwable.printStackTrace();
	}

}
