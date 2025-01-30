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
package net.codecadenza.eclipse.tools.util.eclipse;

import java.util.HashSet;
import java.util.Set;
import net.codecadenza.eclipse.tools.util.os.OperatingSystem;

/**
 * <p>
 * Utility class for getting all necessary plug-ins of an Eclipse RCP application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EclipseRCPPlugins {
	/**
	 * Prevent instantiation
	 */
	private EclipseRCPPlugins() {

	}

	/**
	 * Get all plug-ins for running an Eclipse RCP application
	 * @return a list that contains all plug-ins
	 */
	public static final Set<PluginRecord> getPlugins() {
		final Set<PluginRecord> plugins = new HashSet<>();
		final String osInterface;
		String osExtension;

		// Determine the operating system in order to use the proper SWT plug-ins
		if (OperatingSystem.isWindows()) {
			osInterface = "win32";
			osExtension = osInterface + ".win32.x86";
		}
		else if (OperatingSystem.isMacOS()) {
			osInterface = "cocoa";
			osExtension = osInterface + ".macosx.x86";
		}
		else {
			osInterface = "gtk";
			osExtension = osInterface + ".linux.x86";
		}

		if (OperatingSystem.has64BitArchitecture())
			osExtension += "_64";

		// Add IDs to the set
		plugins.add(new PluginRecord("com.ibm.icu"));
		plugins.add(new PluginRecord("com.sun.jna.platform"));
		plugins.add(new PluginRecord("com.sun.jna"));
		plugins.add(new PluginRecord("jakarta.annotation-api"));
		plugins.add(new PluginRecord("jakarta.inject.jakarta.inject-api"));
		plugins.add(new PluginRecord("org.apache.batik.constants"));
		plugins.add(new PluginRecord("org.apache.batik.css"));
		plugins.add(new PluginRecord("org.apache.batik.i18n"));
		plugins.add(new PluginRecord("org.apache.batik.util"));
		plugins.add(new PluginRecord("org.apache.commons.commons-io"));
		plugins.add(new PluginRecord("org.apache.commons.jxpath"));
		plugins.add(new PluginRecord("org.apache.commons.logging"));
		plugins.add(new PluginRecord("org.apache.felix.scr", "1:true"));
		plugins.add(new PluginRecord("org.apache.xmlgraphics"));
		plugins.add(new PluginRecord("org.eclipse.core.commands"));
		plugins.add(new PluginRecord("org.eclipse.core.contenttype"));
		plugins.add(new PluginRecord("org.eclipse.core.databinding.beans"));
		plugins.add(new PluginRecord("org.eclipse.core.databinding.observable"));
		plugins.add(new PluginRecord("org.eclipse.core.databinding.property"));
		plugins.add(new PluginRecord("org.eclipse.core.databinding"));
		plugins.add(new PluginRecord("org.eclipse.core.expressions"));
		plugins.add(new PluginRecord("org.eclipse.core.jobs"));
		plugins.add(new PluginRecord("org.eclipse.core.runtime", "default:true"));
		plugins.add(new PluginRecord("org.eclipse.e4.core.commands"));
		plugins.add(new PluginRecord("org.eclipse.e4.core.contexts"));
		plugins.add(new PluginRecord("org.eclipse.e4.core.di.annotations"));
		plugins.add(new PluginRecord("org.eclipse.e4.core.di.extensions.supplier"));
		plugins.add(new PluginRecord("org.eclipse.e4.core.di.extensions"));
		plugins.add(new PluginRecord("org.eclipse.e4.core.di"));
		plugins.add(new PluginRecord("org.eclipse.e4.core.services"));
		plugins.add(new PluginRecord("org.eclipse.e4.emf.xpath"));
		plugins.add(new PluginRecord("org.eclipse.e4.ui.bindings"));
		plugins.add(new PluginRecord("org.eclipse.e4.ui.css.core"));
		plugins.add(new PluginRecord("org.eclipse.e4.ui.css.swt.theme"));
		plugins.add(new PluginRecord("org.eclipse.e4.ui.css.swt"));
		plugins.add(new PluginRecord("org.eclipse.e4.ui.di"));
		plugins.add(new PluginRecord("org.eclipse.e4.ui.dialogs"));
		plugins.add(new PluginRecord("org.eclipse.e4.ui.model.workbench"));
		plugins.add(new PluginRecord("org.eclipse.e4.ui.services"));
		plugins.add(new PluginRecord("org.eclipse.e4.ui.swt." + osInterface, "default:false"));
		plugins.add(new PluginRecord("org.eclipse.e4.ui.widgets"));
		plugins.add(new PluginRecord("org.eclipse.e4.ui.workbench.addons.swt"));
		plugins.add(new PluginRecord("org.eclipse.e4.ui.workbench.renderers.swt"));
		plugins.add(new PluginRecord("org.eclipse.e4.ui.workbench.swt"));
		plugins.add(new PluginRecord("org.eclipse.e4.ui.workbench3"));
		plugins.add(new PluginRecord("org.eclipse.e4.ui.workbench"));
		plugins.add(new PluginRecord("org.eclipse.emf.common"));
		plugins.add(new PluginRecord("org.eclipse.emf.databinding"));
		plugins.add(new PluginRecord("org.eclipse.emf.ecore.change"));
		plugins.add(new PluginRecord("org.eclipse.emf.ecore.xmi"));
		plugins.add(new PluginRecord("org.eclipse.emf.ecore"));
		plugins.add(new PluginRecord("org.eclipse.equinox.app"));
		plugins.add(new PluginRecord("org.eclipse.equinox.common", "2:true"));
		plugins.add(new PluginRecord("org.eclipse.equinox.concurrent"));
		plugins.add(new PluginRecord("org.eclipse.equinox.event"));
		plugins.add(new PluginRecord("org.eclipse.equinox.http.service.api"));
		plugins.add(new PluginRecord("org.eclipse.equinox.preferences"));
		plugins.add(new PluginRecord("org.eclipse.equinox.registry"));
		plugins.add(new PluginRecord("org.eclipse.help"));
		plugins.add(new PluginRecord("org.eclipse.jetty.servlet-api"));
		plugins.add(new PluginRecord("org.eclipse.jface.databinding"));
		plugins.add(new PluginRecord("org.eclipse.jface.notifications"));
		plugins.add(new PluginRecord("org.eclipse.jface.text"));
		plugins.add(new PluginRecord("org.eclipse.jface"));
		plugins.add(new PluginRecord("org.eclipse.orbit.xml-apis-ext"));
		plugins.add(new PluginRecord("org.eclipse.osgi.compatibility.state", "default:false"));
		plugins.add(new PluginRecord("org.eclipse.osgi.util"));
		plugins.add(new PluginRecord("org.eclipse.osgi", "1:true"));
		plugins.add(new PluginRecord("org.eclipse.swt." + osExtension, "default:false"));
		plugins.add(new PluginRecord("org.eclipse.swt"));
		plugins.add(new PluginRecord("org.eclipse.text"));
		plugins.add(new PluginRecord("org.eclipse.ui.workbench"));
		plugins.add(new PluginRecord("org.eclipse.ui"));
		plugins.add(new PluginRecord("org.eclipse.urischeme"));
		plugins.add(new PluginRecord("org.osgi.service.cm"));
		plugins.add(new PluginRecord("org.osgi.service.component"));
		plugins.add(new PluginRecord("org.osgi.service.device"));
		plugins.add(new PluginRecord("org.osgi.service.event"));
		plugins.add(new PluginRecord("org.osgi.service.http.whiteboard"));
		plugins.add(new PluginRecord("org.osgi.service.metatype"));
		plugins.add(new PluginRecord("org.osgi.service.prefs"));
		plugins.add(new PluginRecord("org.osgi.service.provisioning"));
		plugins.add(new PluginRecord("org.osgi.service.upnp"));
		plugins.add(new PluginRecord("org.osgi.service.useradmin"));
		plugins.add(new PluginRecord("org.osgi.service.wireadmin"));
		plugins.add(new PluginRecord("org.osgi.util.function"));
		plugins.add(new PluginRecord("org.osgi.util.measurement"));
		plugins.add(new PluginRecord("org.osgi.util.position"));
		plugins.add(new PluginRecord("org.osgi.util.promise"));
		plugins.add(new PluginRecord("org.osgi.util.xml"));

		return plugins;
	}

}
