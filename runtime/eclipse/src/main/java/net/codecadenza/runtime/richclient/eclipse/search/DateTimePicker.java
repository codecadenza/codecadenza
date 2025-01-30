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
package net.codecadenza.runtime.richclient.eclipse.search;

import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.DATE_TIME_PICKER_TITLE;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.getTranslation;

import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import net.codecadenza.runtime.richclient.eclipse.image.ImageCache;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.DateTime;
import org.eclipse.swt.widgets.Shell;

/**
 * <p>
 * Dialog for selecting a date and a time
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DateTimePicker extends Dialog {
	private Date date;
	private DateTime time;
	private DateTime calendar;
	private final boolean displayDateAndTime;

	/**
	 * Constructor
	 * @param parentShell
	 * @param initialDate
	 * @param displayDateAndTime
	 */
	public DateTimePicker(Shell parentShell, Date initialDate, boolean displayDateAndTime) {
		super(parentShell);

		this.displayDateAndTime = displayDateAndTime;
		date = initialDate;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.TitleAreaDialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent);

		calendar = new DateTime(panDialogArea, SWT.CALENDAR);
		calendar.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		if (displayDateAndTime) {
			time = new DateTime(panDialogArea, SWT.TIME);
			time.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, true, true));
		}

		// If the date is set the respective controls should display this date instead of the actual date
		if (date != null) {
			final var gc = new GregorianCalendar();
			gc.setTime(date);

			calendar.setYear(gc.get(Calendar.YEAR));
			calendar.setMonth(gc.get(Calendar.MONTH));
			calendar.setDay(gc.get(Calendar.DAY_OF_MONTH));

			if (displayDateAndTime) {
				time.setHours(gc.get(Calendar.HOUR_OF_DAY));
				time.setMinutes(gc.get(Calendar.MINUTE));
				time.setSeconds(gc.get(Calendar.SECOND));
			}
		}

		return panDialogArea;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == Dialog.OK) {
			GregorianCalendar gc = null;
			final int year = calendar.getYear();
			final int month = calendar.getMonth();
			final int day = calendar.getDay();

			if (displayDateAndTime) {
				final int hour = time.getHours();
				final int minute = time.getMinutes();
				final int second = time.getSeconds();
				gc = new GregorianCalendar(year, month, day, hour, minute, second);
			}
			else
				gc = new GregorianCalendar(year, month, day);

			date = gc.getTime();
		}
		else
			date = new Date();

		super.buttonPressed(buttonId);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
	 */
	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);

		newShell.setImage(ImageCache.getImage(ImageCache.IMG_CALENDAR));
		newShell.setText(getTranslation(DATE_TIME_PICKER_TITLE));
	}

	/**
	 * @return the date
	 */
	public Date getDate() {
		return date;
	}

	/**
	 * @param date the date to set
	 */
	public void setDate(Date date) {
		this.date = date;
	}

}
